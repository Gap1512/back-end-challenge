(in-package :back-end)

(defun start-server (port)
  (clack:clackup (make-clack-app) :port port))

(defvar *config* '("challenge" "lisp" "lisp" "localhost"))

(defroute activities (:get :text/*)
	  (with-connection *config*
	    (to-json (select-dao 'activity))))

(defroute activity (:post "application/json")
	  (with-connection *config*
	    (let* ((json (handler-case
			     (parse (payload-as-string) :as :plist)
			   (error (e)
			     (http-condition 400 "Malformed JSON (~a)!" e))))
		   (act (handler-case (insert-dao
				       (let ((title (getf json :|activityTitle|))
					     (subtitle (getf json :|activitySubtitle|))
					     (sla (getf json :|sla|)))
					 (if (and title subtitle sla)
					     (make-instance 'activity
							    :title title
							    :subtitle subtitle
							    :sla sla)
					     (error "Missing fields"))))
			  (error (e)
			    (http-condition 400 "Invalid Entry (~a)!" e)))))
	      (with-output-to-string (s)
		(format s "Index: ~a" (id act))))))

(defun get-cards (&key (page 0) (per-page 20)
		    (activity-id :null) (patient-name :null)
		    (visit-id :null) (bill-id :null) (to-receive nil)
		    (to-send nil))
  (with-connection *config*
    (query-dao 'card-result "SELECT * FROM getCards($1, $2, $3, $4, $5, $6, $7, $8)"
	       per-page (* page per-page) activity-id patient-name visit-id bill-id to-receive to-send)))

(defun get-sla-status (sla days)
  (cond
    ((> days sla) "DELAYED")
    ((< days (* 0.75 sla)) "WARNING")
    (t "OK")))

(defun get-item (class id)
  (with-connection *config*
    (get-dao class id)))
  
(defun get-patient (id)
  (get-item 'patient id))

(defun get-health-insurance (id)
  (get-item 'health-insurance id))

(defroute cards (:get "text/*" &key (page 0) (perPage 20)
		      (activityId :null) (patientName :null) (visitId :null)
		      (billId :null) (filter "PRIORITY"))
	  (to-json (make-instance 'cards-result
				  :cards-list (get-cards :page page :per-page perPage
							 :activity-id activityId
							 :patient-name patientName
							 :visit-id visitId
							 :bill-id billId
							 :to-receive (string= filter "TO_RECEIVE")
							 :to-send (string= filter "TO_SEND")))))

(defun ensure (type object)
  (with-connection *config*
    (let ((id (getf object :|id|)))
      (if id
	  (get-dao type id)
	  (insert-dao (to-dao (make-instance type) object))))))

(defroute card (:post "application/json")
    (let* ((json (handler-case (parse (payload-as-string) :as :plist)
		   (error (e)
		     (http-condition 400 "Malformed JSON (~a)!" e))))
	   (c (handler-case (ensure 'card json)
		(error (e)
		  (http-condition 400 "Invalid Entry (~a)!" e)))))
      (let ((pendencies (getf json :|pendencies|))
	    (documents (getf json :|documents|))
	    (checklist-items (getf json :|checklistItems|))
	    (card-id (id c)))
	(n-to-n pendencies 'pendency 'cards-pendencies :pendency-id card-id)
	(n-to-n documents 'documents 'cards-documents :document-id card-id)
	(n-to-n checklist-items 'checklist-item 'cards-checklist-item :checklist-item-id card-id)
      (with-output-to-string (s)
	(format s "Index: ~a" card-id)))))

(defun n-to-n (lst type-of-list class-relationship relationship-symbol card-id)
  (with-connection *config*
    (mapcar #'(lambda (item)
		(let ((x (make-instance class-relationship
					   relationship-symbol (id (ensure type-of-list item))
					   :card-id card-id)))
		(insert-dao x)))
		lst)))

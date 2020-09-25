(in-package :back-end)

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
		   (act (insert-dao (make-instance 'activity
				       :title (getf json :|activityTitle|)
				       :subtitle (getf json :|activitySubtitle|)
				       :sla (getf json :|sla|)))))
	      (with-output-to-string (s)
		(format s "Index: ~a" (id act))))))

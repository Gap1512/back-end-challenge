(in-package :back-end)

(defclass activity ()
  ((id :col-type integer :col-identity t :initarg :id :accessor id)
   (title :col-type string :initarg :title :accessor title)
   (subtitle :col-type string :initarg :subtitle :accessor subtitle)
   (sla :col-type integer :initarg :sla :accessor sla))
  (:metaclass dao-class)
  (:keys id)
  (:table-name activities))

(defclass card ()
  ((id :col-type integer :col-identity t :accessor id)
   (creation-date :col-name creationDate :col-type timestamp
		  :initarg :creation-date :accessor creation-date)
   (patient-id :col-name patientId :col-type integer :col-references ((patients id))
	       :initarg :patient-id :accessor patient-id)
   (activity-id :col-name activityId :col-type integer :col-references ((activities id))
		:initarg :activity-id :accessor activity-id)
   (health-insurance-id :col-name healthInsuranceId :col-type integer :col-references ((healthInsurances id))
			:initarg :health-insurance-id :accessor health-insurance-id)
   (visit-id :col-name visitId :col-type integer
	     :initarg :visit-id :accessor visit-id)
   (bill-id :col-name billId :col-type integer :col-references ((bills id))
	    :initarg :bill-id :accessor bill-id))
  (:metaclass dao-class)
  (:keys id)
  (:table-name cards))

(defclass card-result (card)
  ((days-since-created :col-name daysSinceCreated :col-type integer
		       :initarg :days-since-created :accessor days-since-created)
   (sla :col-name sla :col-type integer
	:initarg :sla :accessor sla)
   (bill-type :col-name billType :col-type string
	      :initarg :bill-type :accessor bill-type)
   (total-amount :col-name totalAmount :col-type float
	      :initarg :total-amount :accessor total-amount)
   (n-pends :col-name numberOfPendencies :col-type integer
	    :initarg :n-pends :accessor n-pends)
   (n-open-pends :col-name numberOfOpenPendencies :col-type integer
		 :initarg :n-open-pends :accessor n-open-pends)
   (n-docs :col-name numberOfnumberOfDocuments :col-type integer
	   :initarg :n-docs :accessor n-docs)
   (n-not-received-docs :col-name numberOfNotReceivedDocuments :col-type integer
			:initarg :n-not-received-docs :accessor n-not-received-docs)
   (n-checlist-item :col-name numberOfChecklistItem :col-type integer
		    :initarg :n-checklist-item :accessor n-checlist-item)
   (n-done-checlist-item :col-name numberOfDoneChecklistItem :col-type integer
			 :initarg :n-donechecklist-item :accessor n-done-checlist-item)
   (sla-status :accessor sla-status)
   (patient :accessor patient)
   (health-insurance :accessor health-insurance))
  (:metaclass dao-class))

(defclass health-insurance ()
  ((id :col-type integer :col-identity t :accessor id)
   (name :col-type string :initarg :name :accessor name))
  (:metaclass dao-class)
  (:keys id)
  (:table-name healthInsurances))

(defclass patient ()
  ((id :col-type integer :col-identity t :accessor id)
   (name :col-type string :initarg :name :accessor name))
  (:metaclass dao-class)
  (:keys id)
  (:table-name patients))

(defclass bill ()
  ((id :col-type integer :col-identity t :accessor id)
   (type-id :col-name typeId :col-type integer :col-references ((billTypes id))
	       :initarg :type-id :accessor type-id)
   (total-amount :col-name totalAmount :col-type float
		 :initarg :total-amount :accessor total-amount)
   (type-name :accessor type-name))
  (:metaclass dao-class)
  (:keys id)
  (:table-name bills))

(defclass bill-type ()
  ((id :col-type integer :col-identity t :accessor id)
   (name :col-type string :initarg :name :accessor name))
  (:metaclass dao-class)
  (:keys id)
  (:table-name billTypes))

(defclass cards-pendencies ()
  ((card-id :col-name cardId :col-type integer :col-references ((cards id))
	       :initarg :card-id :accessor card-id)
   (pendency-id :col-name pendencyID :col-type integer :col-references ((pendecies id))
	       :initarg :pendency-id :accessor pendency-id))
  (:metaclass dao-class)
  (:keys id)
  (:table-name cardsPendencies))

(defclass cards-documents ()
  ((card-id :col-name cardId :col-type integer :col-references ((cards id))
	       :initarg :card-id :accessor card-id)
   (document-id :col-name documentID :col-type integer :col-references ((documents id))
	       :initarg :document-id :accessor document-id))
  (:metaclass dao-class)
  (:keys id)
  (:table-name cardsDocuments))

(defclass cards-checklist-item ()
  ((card-id :col-name cardId :col-type integer :col-references ((cards id))
	       :initarg :card-id :accessor card-id)
   (checklist-item-id :col-name checklistItemID :col-type integer :col-references ((checklistItems id))
	       :initarg :checklist-item-id :accessor checklist-item-id))
  (:metaclass dao-class)
  (:keys id)
  (:table-name cardsChecklistItem))

(defclass pendency ()
  ((id :col-type integer :col-identity t :accessor id)
   (openp :col-name open :col-type boolean :initarg :openp :accessor openp))
  (:metaclass dao-class)
  (:keys id)
  (:table-name pendencies))

(defclass documents ()
  ((id :col-type integer :col-identity t :accessor id)
   (not-receivedp :col-name notReceived :col-type boolean :initarg :not-receivedp :accessor not-receivedp))
  (:metaclass dao-class)
  (:keys id)
  (:table-name pendencies))

(defclass checklist-item ()
  ((id :col-type integer :col-identity t :accessor id)
   (donep :col-name done :col-type boolean :initarg :donep :accessor donep))
  (:metaclass dao-class)
  (:keys id)
  (:table-name checklistItems))

(defmacro defjson (class &body definitions)
  (let ((object (gensym)))
    `(defmethod %to-json ((,object ,class))
       (with-slots ,(mapcar #'second definitions) ,object
	 (with-object
	   ,@(mapcar #'(lambda (definition)
			 `(write-key-value ,(first definition)
					   ,(second definition)))
		     definitions))))))

(defjson activity
  ("activityId" id)
  ("activityTitle" title)
  ("activitySubtitle" subtitle)
  ("sla" sla))

(defjson patient
  ("patientId" id)
  ("name" name))

(defjson health-insurance
  ("healthInsuranceId" id)
  ("name" name))

(defmethod initialize-instance :after ((result card-result) &key)
  (with-slots (sla-status sla days-since-created patient patient-id health-insurance health-insurance-id)
      result
    (setf sla-status (get-sla-status sla days-since-created)
	  patient (get-patient patient-id)
	  health-insurance (get-health-insurance health-insurance-id))))

(defjson card-result
  ("daysSinceCreated" days-since-created)
  ("slaStatus" sla-status)
  ("patient" patient)
  ("healthInsurance" health-insurance)
  ("visitId" visit-id)
  ("billId" bill-id)
  ("billType" bill-type)
  ("totalAmount" total-amount)
  ("numberOfPendencies" n-pends)
  ("numberOfOpenPendencies" n-open-pends)
  ("numberOfDocuments" n-docs)
  ("numberOfNotReceivedDocuments" n-not-received-docs)
  ("numberOfChecklistItem" n-checlist-item)
  ("numberOfDoneChecklistItem" n-done-checlist-item))

(defclass cards-result ()
  ((cards-list :initarg :cards-list :accessor cards-list)
   (total-cards-ok :accessor total-cards-ok)
   (total-cards-warning :accessor total-cards-warning)
   (total-cards-delayed :accessor total-cards-delayed)))

(defjson cards-result
  ("cards" cards-list)
  ("totalCardsOk" total-cards-ok)
  ("totalCardsWarning" total-cards-warning)
  ("totalCardsDelayed" total-cards-delayed))

(defmethod initialize-instance :after ((result cards-result) &key)
  (with-slots (cards-list total-cards-ok total-cards-warning total-cards-delayed)
      result
    (loop for card-result in cards-list
       for sla-status = (sla-status card-result)
       counting (string= "OK" sla-status) into ok
       counting (string= "WARNING" sla-status) into warning
       counting (string= "DELAYED" sla-status) into delayed
       finally (setf total-cards-ok ok
		     total-cards-warning warning
		     total-cards-delayed delayed))))

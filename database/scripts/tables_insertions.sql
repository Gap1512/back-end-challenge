INSERT INTO BillTypes (name) VALUES
	('HOSPITALAR'),
	('AMBULATORIAL');
	
INSERT INTO Bills (typeId, totalAmount) VALUES
	(1, 5000.00),
	(2, 3500.00);

INSERT INTO ChecklistItems (done) VALUES
	(false),
	(true);
	
INSERT INTO Documents (notReceived) VALUES
	(false),
	(true);
	
INSERT INTO Pendencies (open) VALUES
	(false),
	(true);

INSERT INTO Patients (name) VALUES
	('Ruan Samuel Luís Dias'),
	('Jaqueline Aline de Paula');
	
INSERT INTO HealthInsurances (name) VALUES
	('Pietra e Joaquim Ltda'),
	('Fabiana e Lavínia Ltda');
	
INSERT INTO Activities (title, subtitle, sla) VALUES
	('Atividade 1', 'Subtítulo 1', 4),
	('Atividade 2', 'Subtítulo 2', 10);
	
INSERT INTO Cards (patientId, activityId, healthInsuranceId, visitId, billId) 
	VALUES
		(1, 1, 1, 1, 1),
		(2, 2, 2, 2, 2);
		
INSERT INTO CardsChecklistItem (cardId, checklistItemId) VALUES
	(1, 1),
	(2, 2);
	
INSERT INTO CardsDocuments (cardId, documentId) VALUES
	(1, 1),
	(2, 2);
	
INSERT INTO CardsPendencies (cardId, pendencyId) VALUES
	(1, 1),
	(2, 2);

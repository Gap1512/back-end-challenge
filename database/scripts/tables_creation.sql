CREATE TABLE IF NOT EXISTS Activities (
       id SERIAL PRIMARY KEY,
       title TEXT NOT NULL,
       subtitle TEXT NOT NULL,
       sla INTEGER NOT NULL,
       CHECK (title <> ''),
       CHECK (subtitle <> '')
);

CREATE TABLE IF NOT EXISTS HealthInsurances (
       id SERIAL PRIMARY KEY,
       name TEXT NOT NULL,
       CHECK (name <> '')
);

CREATE TABLE IF NOT EXISTS Patients (
       id SERIAL PRIMARY KEY,
       name TEXT NOT NULL,
       CHECK (name <> '')
);

CREATE TABLE IF NOT EXISTS BillTypes (
       id SERIAL PRIMARY KEY,
       name TEXT NOT NULL,
       CHECK (name <> '')
);

CREATE TABLE IF NOT EXISTS Pendencies (
       id SERIAL PRIMARY KEY,
       open BOOLEAN NOT NULL
);

CREATE TABLE IF NOT EXISTS Documents (
       id SERIAL PRIMARY KEY,
       notReceived BOOLEAN NOT NULL
);

CREATE TABLE IF NOT EXISTS ChecklistItems (
       id SERIAL PRIMARY KEY,
       done BOOLEAN NOT NULL
);

CREATE TABLE IF NOT EXISTS Bills (
       id SERIAL PRIMARY KEY,
       typeId INTEGER NOT NULL,
       totalAmount REAL NOT NULL,
       FOREIGN KEY (typeId) REFERENCES BillTypes (id)
);

CREATE TABLE IF NOT EXISTS Cards (
       id SERIAL PRIMARY KEY,
       creationDate TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
       patientId INTEGER NOT NULL,
       activityId INTEGER NOT NULL,
       healthInsuranceId INTEGER NOT NULL,
       visitId INTEGER NOT NULL,
       billId INTEGER NOT NULL,
       FOREIGN KEY (patientId) REFERENCES Patients (id),
       FOREIGN KEY (activityId) REFERENCES Activities (id),
       FOREIGN KEY (healthInsuranceId) REFERENCES HealthInsurances (id),
       FOREIGN KEY (billId) REFERENCES Bills (id)
);

CREATE TABLE IF NOT EXISTS CardsPendencies (
       cardId INTEGER NOT NULL,
       pendencyId INTEGER NOT NULL,
       FOREIGN KEY (cardId) REFERENCES Cards (id),
       FOREIGN KEY (pendencyId) REFERENCES Pendencies (id)
);

CREATE TABLE IF NOT EXISTS CardsDocuments (
       cardId INTEGER NOT NULL,
       documentId INTEGER NOT NULL,
       FOREIGN KEY (cardId) REFERENCES Cards (id),
       FOREIGN KEY (documentId) REFERENCES Documents (id)
);

CREATE TABLE IF NOT EXISTS CardsChecklistItem (
       cardId INTEGER NOT NULL,
       checklistItemId INTEGER NOT NULL,
       FOREIGN KEY (cardId) REFERENCES Cards (id),
       FOREIGN KEY (checklistItemId) REFERENCES ChecklistItems (id)
);

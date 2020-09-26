CREATE OR REPLACE FUNCTION getCards(lim int, offs int, 
									act int, pat text, vis int, bil int, 
									toReceive boolean, toSend boolean) 
RETURNS TABLE (id integer,
			   daysSinceCreated integer,
			  sla integer,
			  patientId integer,
			  healthInsuranceId integer,
			  visitId integer,
			  billId integer,
			  billType text,
			  totalAmount real,
			  numberOfPendencies bigint,
			  numberOfOpenPendencies bigint,
			  numberOfnumberOfDocuments bigint,
			  numberOfNotReceivedDocuments bigint,
			  numberOfChecklistItem bigint,
			  numberOfDoneChecklistItem bigint
			  ) AS
$func$
BEGIN
RETURN QUERY 
SELECT c.id, 
	CURRENT_DATE - DATE(c.creationDate),
	a.sla, c.patientId, c.healthInsuranceId,
	c.visitId, c.billId, bt.name, b.totalAmount, 
	COUNT(pend.id),	COUNT(pend.open), COUNT(d.id), COUNT(d.notReceived),
	COUNT(ci.id), COUNT(ci.done)
FROM cards c
	JOIN activities a ON (a.id = c.activityId)
	JOIN bills b ON (b.id = c.billId)
	JOIN billTypes bt ON (bt.id = b.typeId)
    JOIN cardsPendencies cp ON (cp.cardId = c.id)
	JOIN pendencies pend ON (cp.pendencyId = pend.id)
    JOIN cardsDocuments cd ON (cd.cardId = c.id)
	JOIN documents d ON (cd.documentId = d.id)
    JOIN cardsChecklistItem cci ON (cci.cardId = c.id)
	JOIN checklistItems ci ON (cci.checklistItemId = ci.id)
	JOIN patients p ON (c.patientId = p.id)
WHERE 
	(act IS NULL OR c.activityId = act)  AND
	(pat IS NULL OR p.name = pat)        AND 
	(vis IS NULL OR c.visitId = vis)     AND
	(bil IS NULL OR c.billId = bil)      AND
	(NOT toReceive OR d.notReceived)     AND
	(NOT toSend OR ((NOT d.notReceived)  AND (ci.done) AND (NOT pend.open)))
    GROUP BY c.id, bt.name, b.totalamount, a.sla
    LIMIT lim
    OFFSET offs;
END
$func$
LANGUAGE plpgsql;

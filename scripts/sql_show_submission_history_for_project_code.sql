/* ----------------------------------------------------------------------------------------------
VIEW SUBMISSION HISTORY FOR A GIVEN ORGANISATION CODE	

Use these scripts to list the transaction history for project - useful when troubleshooting
missing submissions.
-------------------------------------------------------------------------------------------------*/

-- Set a variable to hold project code
DECLARE @ProjectID VARCHAR(5); 
SET @ProjectID = 'RC900' ;

-- Demographics
SELECT DISTINCT
	'Demographics' AS [Table],
	TransactionId,
	ReceivedDate,
	UserEmail
FROM [TLHC_Reporting].[dbo].[tbTLHCTLHC_Demographics]
WHERE LEFT(SubmittedZipFile,5) = @ProjectID

UNION ALL

-- Invites
SELECT DISTINCT
	'Invites' AS [Table],
	TransactionId,
	ReceivedDate,
	UserEmail
FROM [TLHC_Reporting].[dbo].[tbTLHCTLHC_Pathway_Invite]
WHERE LEFT(SubmittedZipFile,5) = @ProjectID

UNION ALL

-- LHC
SELECT DISTINCT
	'LHC' AS [Table],
	TransactionId,
	ReceivedDate,
	UserEmail
FROM [TLHC_Reporting].[dbo].[tbTLHCTLHC_LungHealthCheck]
WHERE LEFT(SubmittedZipFile,5) = @ProjectID

UNION ALL

-- Measurements
SELECT DISTINCT
	'Measurements' AS [Table],
	TransactionId,
	ReceivedDate,
	UserEmail
FROM [TLHC_Reporting].[dbo].[tbTLHCTLHC_Measurements]
WHERE LEFT(SubmittedZipFile,5) = @ProjectID

UNION ALL

-- Other History
SELECT DISTINCT
	'Other History' AS [Table],
	TransactionId,
	ReceivedDate,
	UserEmail
FROM [TLHC_Reporting].[dbo].[tbTLHCTLHC_OtherHistory]
WHERE LEFT(SubmittedZipFile,5) = @ProjectID

UNION ALL

-- LDCT
SELECT DISTINCT
	'LDCT' AS [Table],
	TransactionId,
	ReceivedDate,
	UserEmail
FROM [TLHC_Reporting].[dbo].[tbTLHCTLHC_Pathway_LDCT]
WHERE LEFT(SubmittedZipFile,5) = @ProjectID

UNION ALL

-- Diagnostics
SELECT DISTINCT
	'Diagnostics' AS [Table],
	TransactionId,
	ReceivedDate,
	UserEmail
FROM [TLHC_Reporting].[dbo].[tbTLHCTLHC_Pathway_Diagnostics]
WHERE LEFT(SubmittedZipFile,5) = @ProjectID

UNION ALL

-- Diagnostics
SELECT DISTINCT
	'Smoking' AS [Table],
	TransactionId,
	ReceivedDate,
	UserEmail
FROM [TLHC_Reporting].[dbo].[tbTLHCTLHC_SmokingCessation]
WHERE LEFT(SubmittedZipFile,5) = @ProjectID
ORDER BY [TransactionId] desc
;
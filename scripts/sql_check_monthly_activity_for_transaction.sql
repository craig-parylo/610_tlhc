/* ----------------------------------------------------------------------------------------------
CHECK FOR ACTIVITY PER MONTH FOR A GIVEN TRANSACTION ID

Use these scripts to identify whether a given transaction id contains activity in the main tables
-------------------------------------------------------------------------------------------------*/

-- Set a variable to hold transaction IDs
DECLARE @TransID INT; 
SET @TransID = 188743;

-- Invites
SELECT DISTINCT
	'Invites' AS [Table],
	CONVERT(varchar(6), CONVERT(date, [First_Letter_Date], 104), 112) AS [calc_YearMon],
	COUNT(ParticipantID) AS [Rows]
FROM [TLHC_Reporting].[dbo].[tbTLHCTLHC_Pathway_Invite]
WHERE TransactionId = @TransID
GROUP BY CONVERT(varchar(6), CONVERT(date, [First_Letter_Date], 104), 112)
ORDER BY [calc_YearMon] desc
;

-- LHC
SELECT DISTINCT
	'LHC' AS [Table],
	CONVERT(varchar(6), CONVERT(date, [LHC_Date], 104), 112) AS [calc_YearMon],
	COUNT(ParticipantID) AS [Rows]
FROM [TLHC_Reporting].[dbo].[tbTLHCTLHC_LungHealthCheck]
WHERE TransactionId = @TransID
GROUP BY CONVERT(varchar(6), CONVERT(date, [LHC_Date], 104), 112)
ORDER BY [calc_YearMon] desc
;

-- LDCT
SELECT DISTINCT
	'LDCT' AS [Table],
	CONVERT(varchar(6), CONVERT(date, [LDCT_Date], 104), 112) AS [calc_YearMon],
	COUNT(ParticipantID) AS [Rows]
FROM [TLHC_Reporting].[dbo].[tbTLHCTLHC_Pathway_LDCT]
WHERE TransactionId = @TransID
GROUP BY CONVERT(varchar(6), CONVERT(date, [LDCT_Date], 104), 112)
ORDER BY [calc_YearMon] desc
;

-- Smoking
SELECT DISTINCT
	'Smoking' AS [Table],
	CONVERT(varchar(6), CONVERT(date, [Date_Offered_Smoking_Cessation], 104), 112) AS [calc_YearMon],
	COUNT(ParticipantID) AS [Rows]
FROM [TLHC_Reporting].[dbo].[tbTLHCTLHC_SmokingCessation]
WHERE TransactionId = @TransID
GROUP BY CONVERT(varchar(6), CONVERT(date, [Date_Offered_Smoking_Cessation], 104), 112)
ORDER BY [calc_YearMon] desc
;
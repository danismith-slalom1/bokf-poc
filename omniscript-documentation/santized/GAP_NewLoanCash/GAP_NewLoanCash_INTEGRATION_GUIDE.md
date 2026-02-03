# GAP_NewLoanCash Integration Guide

## Program: GAP_NewLoanCash
**Purpose**: Integration, deployment, and operational guidance  
**Last Updated**: 2026-02-03

---

## Integration Overview

GAP_NewLoanCash integrates with the cash reconciliation ecosystem by:
- Reading plan position data from POPP database
- Reading transaction activity from SSSA database  
- Generating C1 activity records for cash reconciliation
- Updating position records to track processing

---

## Entry Point Contract

### Program Invocation

**Command Line**:
```bash
# Standard execution
omniscript GAP_NewLoanCash.cbl

# With explicit environment
export RUN_DATE=20240115
export XDAT=/data/batch/output
omniscript GAP_NewLoanCash.cbl
```

**Batch Scheduler Integration**:
```bash
#!/bin/bash
# Daily batch job wrapper

# Set environment
export RUN_DATE=$(date +%Y%m%d)
export XDAT=/app/data/c1activity
export LOG_LEVEL=INFO

# Validate prerequisites
if [ ! -d "$XDAT" ]; then
    echo "ERROR: XDAT directory does not exist: $XDAT"
    exit 1
fi

# Execute program
echo "Starting GAP_NewLoanCash at $(date)"
omniscript /app/omniscript/GAP_NewLoanCash.cbl
EXIT_CODE=$?

# Check results
if [ $EXIT_CODE -eq 0 ]; then
    echo "GAP_NewLoanCash completed successfully"
    # Archive output file
    OUTFILE=$(ls -t $XDAT/OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.*.DAT | head -1)
    if [ -f "$OUTFILE" ]; then
        RECORD_COUNT=$(wc -l < "$OUTFILE")
        echo "Generated $RECORD_COUNT C1 records"
        cp "$OUTFILE" /app/archive/c1activity/
    fi
else
    echo "ERROR: GAP_NewLoanCash failed with exit code $EXIT_CODE"
    # Send alert
    /app/bin/send_alert.sh "GAP_NewLoanCash failed"
fi

exit $EXIT_CODE
```

---

## Input Requirements

### Environment Variables

| Variable | Required | Format | Description | Default/Fallback |
|----------|----------|--------|-------------|------------------|
| `$XDAT` | **YES** | Directory path | Output file directory | None - failure if missing |
| `$RUN-DATE` | NO | YYYYMMDD | Batch processing date | Current date |
| `$LOG_LEVEL` | NO | INFO/WARN/ERROR | Logging verbosity | INFO (if implemented) |

**Validation**:
```bash
# Pre-execution checks
if [ -z "$XDAT" ]; then
    echo "ERROR: XDAT environment variable must be set"
    exit 1
fi

if [ ! -d "$XDAT" ]; then
    echo "ERROR: XDAT directory does not exist: $XDAT"
    exit 1
fi

if [ ! -w "$XDAT" ]; then
    echo "ERROR: No write permission for XDAT directory: $XDAT"
    exit 1
fi
```

---

### Database Prerequisites

#### POPP Table (Plan Position Accounts)
**Required Access**: READ and UPDATE

**Expected Schema**:
```sql
Table: POPP
Fields Required:
  - SECURITYID (String): Must support value 'POOLLOAN3'
  - TRADEDATE (Numeric Date): Format YYYYMMDD
  - Field 008: TradeDate
  - Field 030: RKPlan (Plan identifier)
  - Field 741: Secondary1Buys (numeric dollar amount)
  - Field 877: PriorCashApplied (numeric dollar amount) - MUST BE UPDATABLE
  - Field 1510: TrustAccount (string, 32 chars)
```

**Data Requirements**:
- Position records for POOLLOAN3 security
- Trade dates within last 7 days
- Valid plan identifiers in field 030
- Numeric amounts in fields 741 and 877

**Critical Operations**:
- `poppobj_view()`: Query access with date range filtering
- `poppobj_update()`: Update access for field 877
- Indexes recommended on: SECURITYID, TRADEDATE

---

#### SSSA Table (Trust Transactions - TRUSTTRANS.P1)
**Required Access**: READ only

**Expected Schema**:
```sql
Table: SSSA (TRUSTTRANS.P1)
Fields Required:
  - PLAN (String): Plan identifier
  - SECURITYID (String): Must support value 'POOLLOAN3'
  - DATE (Numeric Date): Format YYYYMMDD
  - Field 009: Transaction Type ('B' = Buy, 'S' = Sell)
  - Field 011: Transaction Source (filter for 'XI')
  - Field 235: Transaction Amount (numeric)
```

**Data Requirements**:
- Activity records for POOLLOAN3 security
- Transaction types 'B' and 'S' with source 'XI'
- Valid transaction amounts

**Critical Operations**:
- `sssaobj_view()`: Query access with plan/security/date filtering
- Indexes recommended on: PLAN, SECURITYID, DATE

---

## Output Specifications

### C1 Activity File

**File Naming Convention**:
```
Pattern: OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.{DATE}.{TIME}.DAT
Example: OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.20240115.020327.DAT

Components:
  - OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET: Static prefix
  - {DATE}: YYYYMMDD format (from OcDate_Current())
  - {TIME}: HHMMSS format (from OcTime_Current())
  - .DAT: File extension
```

**File Location**: `$XDAT` directory

**File Format**: Fixed-length text records, one per line

---

### C1 Record Format

**Record Type**: C100 (Cash Activity)

**Total Length**: 138 bytes minimum

**Field Layout**:
```
Position    Length  Field Name           Value/Source              Format
--------    ------  ------------------   -----------------------   ------
1-4         4       Record Type          'C100'                    Text
5-10        6       Plan ID              RKPlan (POPP.030)         Text
11-30       20      Unused               Spaces                    Text
31-38       8       Effective Date       LastBusiness              Z8
39          1       Unused               Space                     Text
40-71       32      Trust Account        TrustAccount (POPP.1510)  Text
72          1       Unused               Space                     Text
73-92       20      Transaction Code     '000000000000000    2'    Text
93-114      22      Unused               Spaces                    Text
115         1       Sign                 '0'                       Text
116-130     15      Amount               NewLoanUnits (negative)   Z,12V2-
131-133     3       Unused               Spaces                    Text
134-138     5       Activity Code        '00339'                   Text
```

**Example Record**:
```
C100ABC123                    20240114 TRUST_ACCOUNT_ID_HERE_32BYTES 000000000000000    2                      0     -100000.00   00339
```

**Key Characteristics**:
- Amount is always negative (loan purchases offset cash)
- LastBusiness date used as effective date (not trade date)
- Fixed transaction code and activity code
- Trust account from position record

---

## Downstream Consumer Contract

### Cash Reconciliation System

**File Consumption Pattern**:
```bash
# Reconciliation system watches for new C1 files
inotifywait -m "$XDAT" -e create -e moved_to |
while read path action file; do
    if [[ "$file" =~ ^OTDALY\.OMNISCRIPT\.C1\.NEWLOANOFFSET.*\.DAT$ ]]; then
        echo "Processing C1 file: $file"
        /app/reconciliation/import_c1.sh "$path/$file"
    fi
done
```

**Expected Behavior**:
- Reads C1 records sequentially
- Parses fixed-length fields
- Validates record type = 'C100'
- Validates activity code = '00339'
- Processes amounts as negative cash offsets
- Matches to corresponding asset entries

**Error Handling Requirements**:
- Reject records with invalid format
- Reject records with mismatched record type
- Log processing errors for investigation
- Support re-processing of files

---

## Deployment Guide

### System Requirements

**Software**:
- OMNISCRIPT Runtime Environment (version X.X or later)
- Database client libraries (for POPP and SSSA access)
- Shell environment (bash or compatible)

**Hardware**:
- Minimal CPU (I/O bound process)
- 50 MB RAM minimum
- Disk space: 10 MB per 1000 position records

**Network**:
- Database connectivity (TCP/IP)
- Low bandwidth requirements (<1 Mbps)

---

### Installation Steps

1. **Deploy Program File**
   ```bash
   # Copy program to application directory
   cp GAP_NewLoanCash.cbl /app/omniscript/
   chmod 755 /app/omniscript/GAP_NewLoanCash.cbl
   ```

2. **Configure Environment**
   ```bash
   # Create output directory
   mkdir -p /app/data/c1activity
   chmod 755 /app/data/c1activity
   
   # Set environment in profile or batch script
   export XDAT=/app/data/c1activity
   ```

3. **Verify Database Access**
   ```bash
   # Test POPP access
   omniscript -e "poppobj_view(securityid:'TEST'); print poppobj_next();"
   
   # Test SSSA access
   omniscript -e "sssaobj_view(PLAN:'TEST' SECURITYID:'TEST' DATE:20240101); print sssaobj_next();"
   ```

4. **Create Archive Directory**
   ```bash
   mkdir -p /app/archive/c1activity
   chmod 755 /app/archive/c1activity
   ```

5. **Configure Batch Scheduler**
   ```bash
   # Add to crontab (example: daily at 2:00 AM)
   0 2 * * * /app/batch/run_gap_newloancash.sh
   ```

---

### Configuration Management

**Environment Configuration File** (`/app/config/gap_newloancash.env`):
```bash
# GAP_NewLoanCash Configuration

# Output directory for C1 files
export XDAT=/app/data/c1activity

# Batch processing date (leave empty for current date)
# Format: YYYYMMDD
export RUN_DATE=

# Logging level (INFO, WARN, ERROR)
export LOG_LEVEL=INFO

# Archive directory
export ARCHIVE_DIR=/app/archive/c1activity

# Database connection settings (if needed)
export DB_HOST=dbserver.company.com
export DB_PORT=5432
export DB_NAME=trustdb
```

**Load Configuration**:
```bash
# In batch wrapper script
source /app/config/gap_newloancash.env
```

---

## Operational Procedures

### Daily Execution Workflow

1. **Pre-Execution Checks** (Automated)
   - Verify `$XDAT` directory exists and is writable
   - Verify database connectivity
   - Verify previous day's run completed successfully

2. **Execution** (Scheduled)
   - Set `RUN_DATE` (or use default current date)
   - Execute `GAP_NewLoanCash.cbl`
   - Monitor execution time (alert if exceeds threshold)

3. **Post-Execution Validation** (Automated)
   - Verify output file created
   - Verify output file is non-empty (if positions expected)
   - Compare record count to database query
   - Archive output file

4. **Downstream Handoff**
   - Trigger cash reconciliation import
   - Verify reconciliation system consumes file
   - Monitor for reconciliation errors

---

### Monitoring and Alerting

**Success Criteria**:
```bash
# Check if output file exists
OUTFILE=$(ls -t $XDAT/OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.*.DAT 2>/dev/null | head -1)
if [ -z "$OUTFILE" ]; then
    echo "WARNING: No output file generated"
    # Check if zero positions expected
    POSITION_COUNT=$(query_position_count_for_date)
    if [ "$POSITION_COUNT" -gt 0 ]; then
        echo "ALERT: Positions exist but no output file generated"
        send_alert "GAP_NewLoanCash: Missing output file"
    fi
fi

# Verify record count
EXPECTED_COUNT=$(query_unprocessed_positions)
ACTUAL_COUNT=$(wc -l < "$OUTFILE")
if [ "$ACTUAL_COUNT" -ne "$EXPECTED_COUNT" ]; then
    echo "WARNING: Record count mismatch - Expected: $EXPECTED_COUNT, Actual: $ACTUAL_COUNT"
fi
```

**Alert Triggers**:
- Program execution time exceeds 15 minutes
- No output file generated (when positions expected)
- Record count mismatch (>5% variance)
- Database update failures
- File I/O errors

---

### Troubleshooting Guide

#### Problem: No output file generated

**Possible Causes**:
1. `$XDAT` directory doesn't exist or not writable
2. Program crashed before file creation
3. Database connection failed

**Investigation**:
```bash
# Check directory
ls -ld "$XDAT"

# Check for error logs
grep -i "error\|fail" /app/logs/omniscript.log

# Verify database connectivity
omniscript -e "poppobj_view(securityid:'POOLLOAN3'); print poppobj_next();"

# Check for positions in date range
query_positions_for_date_range
```

---

#### Problem: Record count lower than expected

**Possible Causes**:
1. Positions already processed (idempotency working)
2. Database query filtering too restrictive
3. Date range calculation incorrect

**Investigation**:
```bash
# Check how many positions already have field 877 populated
SELECT COUNT(*) FROM POPP
WHERE SECURITYID = 'POOLLOAN3'
  AND TRADEDATE BETWEEN {SevenDaysAgo} AND {LastBusiness}
  AND FIELD877 = FIELD741;  -- Already processed

# Check date range used
grep "SevenDaysAgo\|LastBusiness" /app/logs/omniscript.log
```

---

#### Problem: Incorrect amounts in C1 records

**Possible Causes**:
1. Reversal logic not working correctly
2. SSSA data incomplete or incorrect
3. Field 741 contains wrong data

**Investigation**:
```bash
# Verify SSSA activity for a specific position
SELECT * FROM SSSA
WHERE PLAN = '{RKPlan}'
  AND SECURITYID = 'POOLLOAN3'
  AND DATE = {TradeDate};

# Check field 741 values
SELECT PLAN, TRADEDATE, FIELD741, FIELD877
FROM POPP
WHERE SECURITYID = 'POOLLOAN3'
  AND TRADEDATE = {specific_date};
```

---

## Integration Testing

### Test Scenarios

#### Test 1: End-to-End Integration
**Objective**: Verify complete workflow from database to reconciliation

**Steps**:
1. Insert test positions in POPP with known values
2. Insert corresponding SSSA activity (if testing reversals)
3. Execute program
4. Verify output file generated
5. Verify C1 records contain correct values
6. Verify POPP field 877 updated
7. Verify reconciliation system processes file

**Expected Results**:
- Output file exists with correct naming
- Record count matches unprocessed positions
- Amounts match expected values (netted if reversals)
- Database updated correctly
- Reconciliation system imports without errors

---

#### Test 2: Idempotency
**Objective**: Verify program can be safely re-run

**Steps**:
1. Execute program (first run)
2. Note output file record count
3. Execute program again (second run)
4. Note output file record count

**Expected Results**:
- First run: N records written
- Second run: 0 records written (all already processed)
- No duplicate C1 records in reconciliation

---

#### Test 3: Reversal Handling
**Objective**: Verify buy/sell netting logic

**Steps**:
1. Insert position with Secondary1Buys = $100,000
2. Insert SSSA activity: $100,000 buy + $30,000 sell (same date)
3. Execute program
4. Verify C1 record amount

**Expected Results**:
- C1 record shows -$70,000 (net amount)
- Field 877 updated to $70,000

---

## Performance Considerations

### Scalability

**Current Performance**:
- 50-200 positions: 2-5 minutes
- 500 positions: 10-15 minutes
- Linear scaling based on position count

**Bottlenecks**:
- Database query performance (poppobj_view, sssaobj_view)
- Sequential processing (one position at a time)
- File I/O (one write per position)

**Optimization Options**:
1. Add database indexes on SECURITYID and TRADEDATE
2. Batch file writes (accumulate records in memory)
3. Parallelize position processing (requires code refactoring)
4. Pre-aggregate SSSA reversals in separate job

---

## Disaster Recovery

### Backup and Recovery

**Output Files**:
```bash
# Archive output files daily
0 3 * * * /app/batch/archive_c1_files.sh

# Archive script
#!/bin/bash
ARCHIVE_DIR=/app/archive/c1activity/$(date +%Y%m%d)
mkdir -p "$ARCHIVE_DIR"
mv $XDAT/OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.*.DAT "$ARCHIVE_DIR/"
```

**Database Recovery**:
- Field 877 updates are idempotent
- Can re-run program after database restore
- Existing C1 files should be reconciled manually

**Failure Recovery**:
```bash
# If program crashes mid-execution
# 1. Check which positions were updated
SELECT COUNT(*) FROM POPP
WHERE SECURITYID = 'POOLLOAN3'
  AND TRADEDATE BETWEEN {SevenDaysAgo} AND {LastBusiness}
  AND FIELD877 = FIELD741;

# 2. Compare to output file record count
wc -l $XDAT/OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.*.DAT

# 3. If mismatch, investigate and potentially re-run
# 4. Idempotency ensures already-processed records are skipped
```

---

## Security Considerations

### Access Control

**Program Execution**:
- Restrict execution to batch service account
- No direct user access to program

**Database Access**:
- READ access to POPP (fields 008, 030, 741, 877, 1510)
- UPDATE access to POPP (field 877 only)
- READ access to SSSA (fields 009, 011, 235)
- No DELETE or DROP permissions

**File System Access**:
- WRITE access to `$XDAT` directory
- READ access for downstream systems
- No direct user access to output files

**Environment Variables**:
- Protect `$XDAT` from unauthorized modification
- Validate `$RUN-DATE` format to prevent injection

---

## Compliance and Auditing

### Audit Trail

**Database Changes**:
- All POPP field 877 updates are auditable
- Track when positions are marked as processed
- Compare to C1 output files

**File Generation**:
- Output files timestamped with creation time
- File naming convention supports chronological ordering
- Archive files for regulatory retention period

**Reconciliation**:
- C1 records provide complete transaction history
- Can reconstruct cash offset activity from archived files
- Tie back to source POPP positions via plan ID and date

---

## Support and Maintenance

### Change Management

**Adding New Fields**:
1. Update `OcText_Set()` calls to include new field
2. Update C1 record format documentation
3. Coordinate with downstream systems for format changes
4. Test with sample data before deployment

**Modifying Date Range**:
1. Update `OcDate_AddDays()` parameter
2. Document new lookback window
3. Test with various date scenarios
4. Communicate change to operations team

**Database Schema Changes**:
1. Verify field numbers remain stable
2. Add new field handling if schema extends
3. Test database queries after schema change
4. Update documentation

---

## Contact Information

**Program Owner**: [Department Name]  
**Technical Support**: [Support Team]  
**Database Team**: [DBA Team]  
**Operations Team**: [Ops Team]

**Escalation Path**:
1. Level 1: Operations team (file/environment issues)
2. Level 2: Technical support (program logic issues)
3. Level 3: Development team (code changes)
4. Level 4: Database team (schema/query issues)

---

*This integration guide provides comprehensive guidance for deploying and operating GAP_NewLoanCash in production environments.*  
*Last Updated*: 2026-02-03

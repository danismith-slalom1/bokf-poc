# GAP_NEWLOANCASH Integration Guide

## Overview

This document describes all external interfaces, dependencies, and integration points for the GAP_NewLoanCash program, including entry point contracts, database dependencies, file interfaces, and deployment requirements.

---

## 1. Entry Point Contract

### Program Invocation

| Attribute | Value |
|---|---|
| **Program Name** | GAP_NewLoanCash |
| **Source File** | GAP_NewLoanCash.txt |
| **Language** | OmniScript |
| **Invocation** | OmniScript batch execution |
| **Parameters** | None (uses environment variables) |
| **Pre-conditions** | Environment variables set, database access available, output directory exists |
| **Post-conditions** | C1 activity file created, POPP records updated with idempotency markers |

### Environment Variables Required

| Variable | Required | Description | Example |
|---|---|---|---|
| `$XDAT` | **YES** | Base directory for output data files | `/data/omniscript/output/` |
| `$RUN-DATE` | Optional | Processing run date (YYYYMMDD format) | `20240925` |

**Notes**:
- If `$RUN-DATE` is not set or contains an invalid date, the program falls back to the current system date
- `$XDAT` must point to a valid, writable directory

---

## 2. Database Dependencies

### POPP Object (Plan Position)

| Attribute | Value |
|---|---|
| **Object Name** | poppobj |
| **Access Type** | Read/Write |
| **Security Filter** | `POOLLOAN3` |
| **Date Range** | Last 7 calendar days to last business day |
| **Operations** | view, next, de, numde, setde, update |

#### Data Elements Used

| DE# | Name | Type | Access | Purpose |
|---|---|---|---|---|
| 008 | Trade Date | Numeric | Read | Position trade date |
| 030 | Plan ID | String | Read | Plan identifier (6 chars) |
| 741 | Secondary1 Buys | Numeric | Read | Buy amount for the position |
| 877 | UDF1 (Prior Cash Applied) | Numeric | Read/Write | Idempotency marker |
| 01510 | Trust Account | String | Read | Trust account identifier (32 chars) |

#### View Parameters

```
poppobj_view(
    securityid: 'POOLLOAN3',
    datelo: SevenDaysAgo,    -- RunDate - 7 calendar days
    datehi: LastBusiness      -- RunDate - 1 business day
)
```

### SSSA Object (Secondary Security Sub-Account)

| Attribute | Value |
|---|---|
| **Object Name** | sssaobj |
| **Access Type** | Read-only |
| **Called From** | CHECK.SSSA routine |
| **Operations** | view, next, de, numde |

#### Data Elements Used

| DE# | Name | Type | Access | Purpose |
|---|---|---|---|---|
| 009 | Buy/Sell Indicator | String | Read | `B` = Buy, `S` = Sell/Reversal |
| 011 | Transaction Type | String | Read | Filter: only `XI` processed |
| 235 | Amount | Numeric | Read | Transaction dollar amount |

#### View Parameters

```
sssaobj_view(
    PLAN: RKPlan,              -- From POPP DE 030
    SECURITYID: 'POOLLOAN3',
    DATE: TradeDate            -- From POPP DE 008
)
```

---

## 3. File Interface

### Output File

| Attribute | Value |
|---|---|
| **Type** | Sequential output file |
| **Format** | Fixed-width text records |
| **File Handle** | OcFile1 |
| **Mode** | OUTPUT (write, creates new) |

#### File Naming Convention

```
{$XDAT}\OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.{YYYYMMDD}.{HHMMSS}.DAT
```

| Component | Source | Format |
|---|---|---|
| Base path | `$XDAT` environment variable | Directory path |
| Prefix | Hardcoded | `OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET.` |
| Date stamp | `OcDate_Current()` | Z8 (YYYYMMDD) |
| Time stamp | `OcTime_Current()` | Z6 (HHMMSS) |
| Extension | Hardcoded | `.DAT` |

#### Output Record Format (C1 Activity)

Fixed-width record, minimum 138 characters:

| Position | Length | Field | Source | Description |
|---|---|---|---|---|
| 1-4 | 4 | Record Type | `C100` | C1 activity record identifier |
| 5-10 | 6 | Plan ID | POPP DE 030 | Investment plan identifier |
| 11-30 | 20 | (Filler) | Spaces | Unused positions |
| 31-38 | 8 | Activity Date | LastBusiness | Effective date (Z8 format) |
| 39 | 1 | (Filler) | Space | Unused position |
| 40-71 | 32 | Trust Account | POPP DE 01510 | Trust account identifier |
| 72 | 1 | (Filler) | Space | Unused position |
| 73-92 | 20 | Control Flags | `000000000000000    2` | Hardcoded flags (position 92 = '2') |
| 93-114 | 22 | (Filler) | Spaces | Unused positions |
| 115 | 1 | Processing Flag | `0` | Hardcoded processing indicator |
| 116-130 | 15 | Offset Amount | NewLoanUnits | Signed decimal (Z,12V2- format) |
| 131-133 | 3 | (Filler) | Spaces | Unused positions |
| 134-138 | 5 | Transaction Code | `00339` | Cash offset transaction code |

#### Downstream Consumer

The output `.DAT` file is consumed by the C1 activity loading process for cash reconciliation. The file prefix `OTDALY.OMNISCRIPT.C1.NEWLOANOFFSET` identifies it as an OmniScript-generated C1 new loan offset file.

---

## 4. Called Programs / External Dependencies

| Program | Type | Notes |
|---|---|---|
| None | N/A | This program does not call external programs |

The program is self-contained; it uses only OmniScript built-in functions and database objects.

---

## 5. Built-in Function Dependencies

| Function | Lines Used | Purpose |
|---|---|---|
| `OcLVar_Define` | 14-15 | Declare local variables |
| `OcText_string` | 16 | Concatenate strings for filename |
| `OCTEXT_GETENV` | 16, 21 | Read environment variables |
| `OcFmt` / `OcFMT` | 17, 47, 51 | Format numeric values to strings |
| `OcDate_Current` | 17, 26, 27 | Get current system date |
| `OcTime_Current` | 17 | Get current system time |
| `OcShow` | 18, 29 | Display/log values |
| `OcFile1_Open` | 19 | Open output file |
| `OcFile1_Write` | 53 | Write record to output file |
| `octext_tonum` | 21 | Convert text to numeric |
| `octext_getenv` | 21 | Read environment variable (lowercase variant) |
| `OcDate_Valid` | 22 | Validate date value |
| `OcDate_AddDays` | 23, 26 | Add calendar days to date |
| `OcDate_AddBusDays` | 24, 27 | Add business days to date |
| `OcText_Set` | 45-52 | Set text at specific position in buffer |

---

## 6. System Requirements

### Runtime Environment

| Requirement | Value |
|---|---|
| **Runtime** | OmniScript interpreter |
| **Database Access** | POPP (read/write), SSSA (read) |
| **File System** | Write access to `$XDAT` directory |
| **Business Day Calendar** | Required for `OcDate_AddBusDays` |

### Scheduling

| Attribute | Value |
|---|---|
| **Typical Schedule** | Daily batch processing |
| **Run Window** | After POPP positions are loaded, before C1 reconciliation |
| **Dependencies** | POPP POOLLOAN3 positions must be current; SSSA transactions must be loaded |
| **Idempotency** | Safe to re-run; skips already-processed records via UDF1 check |

### Permissions

| Resource | Permission | Purpose |
|---|---|---|
| POPP database | Read + Write | Read positions, update UDF1 (DE 877) |
| SSSA database | Read | Read transaction records |
| `$XDAT` directory | Write | Create output .DAT file |
| `$RUN-DATE` | Read | Environment variable access |
| `$XDAT` | Read | Environment variable access |

---

## 7. Data Flow Summary

```
Environment Variables ($XDAT, $RUN-DATE)
            │
            ▼
┌───────────────────────┐
│  GAP_NewLoanCash      │
│                       │
│  POPP DB ──read──►    │
│  (POOLLOAN3)          │
│         │             │
│         ▼             │
│  SSSA DB ──read──►    │──write──► C1 Activity File (.DAT)
│  (XI transactions)    │
│         │             │
│         ▼             │
│  POPP DB ◄──write──   │
│  (UDF1 update)        │
└───────────────────────┘
```

---

## 8. Deployment Checklist

- [ ] OmniScript interpreter available and configured
- [ ] `$XDAT` environment variable set to valid writable directory
- [ ] `$RUN-DATE` environment variable set (optional; defaults to current date)
- [ ] POPP database accessible with read/write permissions
- [ ] SSSA database accessible with read permissions
- [ ] Business day calendar configured for `OcDate_AddBusDays`
- [ ] POPP POOLLOAN3 positions loaded for the target date range
- [ ] SSSA transaction records loaded (for reversal detection)
- [ ] Downstream C1 processing configured to read from `$XDAT` output directory

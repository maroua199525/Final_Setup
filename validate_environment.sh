#!/bin/bash
#########################################################################
# Environment Validation Script - Check if all datasets are properly set up
# Ensures consistent environment before students start exercises
#########################################################################

echo "🔍 JCL Framework - Environment Validation"
echo "=========================================="
echo "Checking essential datasets and programs for Tasks 1, 2, 3 & File Copy..."
echo ""

# Check if dataset manager is working
if ! ./dataset_manager.sh list >/dev/null 2>&1; then
    echo "❌ Dataset manager not working. Run './setup_datasets.sh' first"
    exit 1
fi

# Essential datasets for Tasks 1, 2, 3 & File Copy
REQUIRED_DATASETS=(
    "TRANSACTIONS.INPUT"
    "TRANSACTIONS.VALIDATED"
    "ACCOUNTS.MASTER"
    "CUSTOMERS.MASTER"
    "STUDENT.INPUT.DATA"
    "STUDENT.OUTPUT.DATA"
)

echo "📋 Checking required datasets..."
MISSING_COUNT=0

for dataset in "${REQUIRED_DATASETS[@]}"; do
    if ./dataset_manager.sh list | grep -q "$dataset"; then
        echo "✅ $dataset - OK"
    else
        echo "❌ $dataset - MISSING"
        ((MISSING_COUNT++))
    fi
done

echo ""
echo "📁 Checking data files..."
DATA_FILES=(
    "datasets/transactions_input.dat"
    "datasets/accounts_master.dat"
    "datasets/customers_master.dat"
    "datasets/student_input_data.dat"
)

for file in "${DATA_FILES[@]}"; do
    if [ -f "$file" ]; then
        echo "✅ $file - OK ($(wc -l < "$file") lines)"
    else
        echo "❌ $file - MISSING"
        ((MISSING_COUNT++))
    fi
done

echo ""
echo "🔧 Checking essential COBOL programs..."
PROGRAMS=(
    "programs/batch_validator.cbl"
    "programs/account_updater.cbl"
    "programs/customer_reporter.cbl"
    "programs/5-file_copy.cbl"
    "programs/hello_world.cbl"
)

for program in "${PROGRAMS[@]}"; do
    if [ -f "$program" ]; then
        echo "✅ $program - OK"
    else
        echo "❌ $program - MISSING"
        ((MISSING_COUNT++))
    fi
done

echo ""
echo "📄 Checking essential JCL jobs..."
JOBS=(
    "jobs/batch_validator.jcl"
    "jobs/account_updater.jcl"
    "jobs/customer_reporter.jcl"
    "jobs/file_copy.jcl"
    "jobs/hello_world.jcl"
    "jobs/banking_workflow.jcl"
)

for job in "${JOBS[@]}"; do
    if [ -f "$job" ]; then
        echo "✅ $job - OK"
    else
        echo "❌ $job - MISSING"
        ((MISSING_COUNT++))
    fi
done

echo ""
echo "🎯 Checking standardized PGM names..."
PGM_CHECKS=(
    "jobs/batch_validator.jcl:VALIDATOR"
    "jobs/account_updater.jcl:UPDATER"
    "jobs/customer_reporter.jcl:REPORTER"
    "jobs/file_copy.jcl:FILECOPY"
    "jobs/hello_world.jcl:HELLO"
    "jobs/banking_workflow.jcl:VALIDATOR"
    "jobs/banking_workflow.jcl:UPDATER"
    "jobs/banking_workflow.jcl:REPORTER"
)

for check in "${PGM_CHECKS[@]}"; do
    file="${check%:*}"
    expected_pgm="${check#*:}"
    if [ -f "$file" ]; then
        if grep -q "EXEC PGM=$expected_pgm" "$file"; then
            echo "✅ $file uses standardized PGM=$expected_pgm"
        else
            echo "❌ $file does not use standardized PGM=$expected_pgm"
            ((MISSING_COUNT++))
        fi
    fi
done

echo ""
echo "📊 Validation Summary:"
echo "====================="
if [ $MISSING_COUNT -eq 0 ]; then
    echo "🎉 Essential Environment is READY!"
    echo "✅ All essential datasets allocated"
    echo "✅ All data files present"
    echo "✅ All COBOL programs available"
    echo "✅ All JCL jobs ready"
    echo "✅ Standardized PGM names verified"
    echo ""
    echo "🚀 Students can now start essential exercises!"
    echo ""
    echo "Quick test commands:"
    echo "  ./jcl_parser.sh jobs/hello_world.jcl"
    echo "  ./jcl_parser.sh jobs/file_copy.jcl"
    echo "  ./jcl_parser.sh jobs/batch_validator.jcl"
    echo "  ./jcl_parser.sh jobs/account_updater.jcl"
    echo "  ./jcl_parser.sh jobs/customer_reporter.jcl"
    echo "  ./jcl_parser.sh jobs/banking_workflow.jcl"
    echo ""
    echo "📋 Tasks available:"
    echo "  • Hello World: Basic COBOL demonstration"
    echo "  • File Copy: Basic file operations"
    echo "  • Task 1: Transaction validation (VALIDATOR)"
    echo "  • Task 2: Account updates (UPDATER)"
    echo "  • Task 3: Customer reporting (REPORTER)"
    echo "  • Final Task: Complete banking workflow (VALIDATOR→UPDATER→REPORTER)"
else
    echo "⚠️  Environment has $MISSING_COUNT missing components"
    echo "🔧 Run './setup_datasets.sh' to fix missing datasets"
    echo "📚 Check documentation for missing programs/jobs"
    echo "💡 Ensure all PGM names are standardized"
    exit 1
fi
#!/bin/bash
#########################################################################
# Dataset Setup Script - Essential datasets for Tasks 1, 2, 3 & File Copy
# Streamlined setup for required exercises only
#########################################################################

echo "🚀 JCL Framework - Essential Dataset Setup"
echo "=========================================="
echo "Setting up required datasets for Tasks 1, 2, 3 and File Copy..."
echo ""

# Initialize dataset manager
./dataset_manager.sh init

echo "📁 Allocating Task 1 (Batch Validator) Datasets..."
./dataset_manager.sh allocate TRANSACTIONS.INPUT PS 2048 100
./dataset_manager.sh allocate TRANSACTIONS.VALIDATED PS 2048 100

echo "📁 Allocating Task 2 (Account Updater) Datasets..."
./dataset_manager.sh allocate ACCOUNTS.MASTER PS 1024 50

echo "📁 Allocating Task 3 (Customer Reporter) Datasets..."
./dataset_manager.sh allocate CUSTOMERS.MASTER PS 1024 50

echo "📁 Allocating File Copy Exercise Datasets..."
./dataset_manager.sh allocate STUDENT.INPUT.DATA PS 1024 50
./dataset_manager.sh allocate STUDENT.OUTPUT.DATA PS 1024 50

echo "📁 Copying Sample Data to Datasets..."
# Copy sample data to ensure consistent starting point
if [ -f "data/transactions.txt" ]; then
    cp data/transactions.txt datasets/transactions_input.dat
    echo "✓ Sample transactions copied to TRANSACTIONS.INPUT"
fi

if [ -f "data/accounts.txt" ]; then
    cp data/accounts.txt datasets/accounts_master.dat
    echo "✓ Sample accounts copied to ACCOUNTS.MASTER"
fi

if [ -f "data/customers.txt" ]; then
    cp data/customers.txt datasets/customers_master.dat
    echo "✓ Sample customers copied to CUSTOMERS.MASTER"
fi

if [ -f "data/sample_input.txt" ]; then
    cp data/sample_input.txt datasets/student_input_data.dat
    echo "✓ Sample input data copied to STUDENT.INPUT.DATA"
fi

echo ""
echo "📋 Essential Dataset Summary:"
echo "============================="
./dataset_manager.sh list

echo ""
echo "✅ Essential dataset setup complete!"
echo "📚 Streamlined setup for required tasks only"
echo ""
echo "🎯 Ready for essential JCL exercises:"
echo "  • Task 1: Batch Validator (TRANSACTIONS.INPUT → TRANSACTIONS.VALIDATED)"
echo "  • Task 2: Account Updater (ACCOUNTS.MASTER + TRANSACTIONS.VALIDATED)"
echo "  • Task 3: Customer Reporter (CUSTOMERS.MASTER)"
echo "  • File Copy: Basic file operations (STUDENT.INPUT.DATA → STUDENT.OUTPUT.DATA)"
echo ""
echo "💡 Only essential datasets allocated - faster setup, cleaner environment!"
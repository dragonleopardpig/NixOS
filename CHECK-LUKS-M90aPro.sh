#!/usr/bin/env bash
# Run this script on the M90aPro machine to understand the LUKS setup

echo "=== Checking LUKS devices ==="
echo ""
echo "1. All block devices:"
lsblk -f
echo ""
echo "=== Checking UUIDs ==="
echo ""
echo "2. Looking for 51c7cb8c-d514-40e1-8286-0185987e196c:"
sudo blkid | grep -i "51c7cb8c-d514-40e1-8286-0185987e196c"
echo ""
echo "3. Looking for 03d74679-bc49-4934-a605-5c0ce7a726cf:"
sudo blkid | grep -i "03d74679-bc49-4934-a605-5c0ce7a726cf"
echo ""
echo "4. Looking for de07a992-0eeb-47b6-93d5-963d73ef7d9a (swap):"
sudo blkid | grep -i "de07a992-0eeb-47b6-93d5-963d73ef7d9a"
echo ""
echo "=== Active LUKS mappings ==="
echo ""
echo "5. Current LUKS devices:"
sudo dmsetup ls --tree
echo ""
echo "6. LUKS status:"
ls /dev/mapper/
echo ""
echo "7. What is currently used as swap:"
swapon --show
cat /proc/swaps

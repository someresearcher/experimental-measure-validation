#!/bin/bash

RDS_INSTANCE_NAME="maria-db"

RDS_FIRST_REPLICA_NAME="first-read-replica"

aws rds create-db-instance-read-replica \
    --db-instance-identifier "${RDS_FIRST_REPLICA_NAME}" \
    --source-db-instance-identifier "${RDS_INSTANCE_NAME}" \
    --allocated-storage 20 \
    --max-allocated-storage 20 \
    --availability-zone us-east-2a

RDS_SECOND_REPLICA_NAME="second-read-replica"

aws rds create-db-instance-read-replica \
    --db-instance-identifier "${RDS_SECOND_REPLICA_NAME}" \
    --source-db-instance-identifier "${RDS_INSTANCE_NAME}" \
    --allocated-storage 20 \
    --max-allocated-storage 20 \
    --availability-zone us-east-2b

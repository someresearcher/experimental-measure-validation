#!/bin/bash

cd a_provisioning

TERRAFORM_OUTPUT=$(terraform output -json)

JMETER_HOST_IP=$(echo $TERRAFORM_OUTPUT | jq '.["jmeter_host_ip"].value[0]')
JMETER_HOST_IP=$(echo "${JMETER_HOST_IP//[\"\'\`]/}")

SSH_KEY_PAIR_NAME=$(echo $TERRAFORM_OUTPUT | jq '.["ssh_key_pair_name"].value')
SSH_KEY_PAIR_NAME=$(echo "${SSH_KEY_PAIR_NAME//[\"\'\`]/}")

cd ..

echo $(kubectl -n teastore-namespace -ojson get service teastore-webui | jq '.["status"].loadBalancer.ingress[0].hostname')
INTERNAL_LOAD_BALANCER=$(kubectl -n teastore-namespace -ojson get service teastore-webui | jq '.["status"].loadBalancer.ingress[0].hostname')

cd c_jmeter
scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} teastore_browse_home.jmx jmeter@${JMETER_HOST_IP}:~/teastore_browse_home.jmx
scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} teastore_browse_login.jmx jmeter@${JMETER_HOST_IP}:~/teastore_browse_login.jmx
scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} teastore_browse_listProducts.jmx jmeter@${JMETER_HOST_IP}:~/teastore_browse_listProducts.jmx
scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} teastore_browse_lookAtProduct.jmx jmeter@${JMETER_HOST_IP}:~/teastore_browse_lookAtProduct.jmx
scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} teastore_browse_addProductToCart.jmx jmeter@${JMETER_HOST_IP}:~/teastore_browse_addProductToCart.jmx
scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} teastore_browse_logout.jmx jmeter@${JMETER_HOST_IP}:~/teastore_browse_logout.jmx
cd ..

cat >run_experiment.sh <<XEOF

TIMESTAMP=\$(date +%Y%m%d_%H%M%S)

cat >run_remotely.sh <<EOF
jmeter -n -Jhostname=${INTERNAL_LOAD_BALANCER} -Jport=80 -Jnum_user=10 -Jramp_up=10 -Jruns=50 -t teastore_browse.jmx -l "\${TIMESTAMP}_warmup.csv"
jmeter -n -Jhostname=${INTERNAL_LOAD_BALANCER} -Jport=80 -Jnum_user=10 -Jramp_up=10 -Jruns=1000 -t teastore_browse.jmx -l "\${TIMESTAMP}_browse_run.csv"

jmeter -n -Jhostname=${INTERNAL_LOAD_BALANCER} -Jport=80 -Jnum_user=10 -Jramp_up=10 -Jruns=100 -t teastore_browse_home.jmx -l "\${TIMESTAMP}_home_run.csv"
jmeter -n -Jhostname=${INTERNAL_LOAD_BALANCER} -Jport=80 -Jnum_user=10 -Jramp_up=10 -Jruns=100 -t teastore_browse_login.jmx -l "\${TIMESTAMP}_login_run.csv"
jmeter -n -Jhostname=${INTERNAL_LOAD_BALANCER} -Jport=80 -Jnum_user=10 -Jramp_up=10 -Jruns=100 -t teastore_browse_listProducts.jmx -l "\${TIMESTAMP}_listProducts_run.csv"
jmeter -n -Jhostname=${INTERNAL_LOAD_BALANCER} -Jport=80 -Jnum_user=10 -Jramp_up=10 -Jruns=100 -t teastore_browse_lookAtProduct.jmx -l "\${TIMESTAMP}_lookAtProduct_run.csv"
jmeter -n -Jhostname=${INTERNAL_LOAD_BALANCER} -Jport=80 -Jnum_user=10 -Jramp_up=10 -Jruns=100 -t teastore_browse_addProductToCart.jmx -l "\${TIMESTAMP}_addProductToCart_run.csv"
jmeter -n -Jhostname=${INTERNAL_LOAD_BALANCER} -Jport=80 -Jnum_user=10 -Jramp_up=10 -Jruns=100 -t teastore_browse_logout.jmx -l "\${TIMESTAMP}_logout_run.csv"
EOF

ssh -i ~/.ssh/${SSH_KEY_PAIR_NAME} jmeter@${JMETER_HOST_IP} < ./run_remotely.sh

scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} jmeter@${JMETER_HOST_IP}:~/\${TIMESTAMP}_warmup.csv ./\${TIMESTAMP}_warmup.csv
scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} jmeter@${JMETER_HOST_IP}:~/\${TIMESTAMP}_browse_run.csv ./\${TIMESTAMP}_browse_run.csv
scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} jmeter@${JMETER_HOST_IP}:~/\${TIMESTAMP}_home_run.csv ./\${TIMESTAMP}_home_run.csv
scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} jmeter@${JMETER_HOST_IP}:~/\${TIMESTAMP}_login_run.csv ./\${TIMESTAMP}_login_run.csv
scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} jmeter@${JMETER_HOST_IP}:~/\${TIMESTAMP}_listProducts_run.csv ./\${TIMESTAMP}_listProducts_run.csv
scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} jmeter@${JMETER_HOST_IP}:~/\${TIMESTAMP}_lookAtProduct_run.csv ./\${TIMESTAMP}_lookAtProduct_run.csv
scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} jmeter@${JMETER_HOST_IP}:~/\${TIMESTAMP}_addProductToCart_run.csv ./\${TIMESTAMP}_addProductToCart_run.csv
scp -i ~/.ssh/${SSH_KEY_PAIR_NAME} jmeter@${JMETER_HOST_IP}:~/\${TIMESTAMP}_logout_run.csv ./\${TIMESTAMP}_logout_run.csv
XEOF
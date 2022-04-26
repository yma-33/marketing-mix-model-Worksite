cat - > ./vertica.ini <<EOF
[DEFAULT]
database=advana
port=443
user=$MMM_USER
host=$VERTICA_HOST
password=$MMM_PASS
EOF

#set-aws-credentials vertica.ini data-scientist
#source ./aws-credentials.sh

cd src/ETL/conversion/

vsql -d advana -h $VERTICA_HOST -p 443 -U $MMM_USER -w $MMM_PASS -v ON_ERROR_STOP=on -t -f conversion_Tera.sql

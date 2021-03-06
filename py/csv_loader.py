#!/usr/bin/python

import psycopg2
import os

#def insert_csv_data():

if __name__ == '__main__':
    create_table = "CREATE TABLE aquarium_data (id SERIAL PRIMARY KEY, observed_at TIMESTAMP, ph_read FLOAT(2), temp_read FLOAT(2),lux_read FLOAT(2));"
    conn = None
    try:
        # connect to the PostgreSQL database
        conn = psycopg2.connect("host=" + os.environ["AWS_PG_HOST"] + " dbname=aquarium user=" + os.environ["AWS_PG_USER"] + " password=" + os.environ["AWS_PG_PW"])
        # create a new cursor
        cur = conn.cursor()
        cur.execute(create_table)
        with open('/home/michael/Documents/aqua_reports/20180721.csv', 'r') as f:
            cur.copy_from(f, 'aquarium_data', sep=',', columns=('observed_at','ph_read','temp_read','lux_read'))

        conn.commit()
    except (Exception, psycopg2.DatabaseError) as error:
        print(error)
    finally:
        if conn is not None:
            conn.close()

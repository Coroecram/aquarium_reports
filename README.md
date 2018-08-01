# Aqua Report
Aqua report is a Dockerized Shiny Application. The main branch is 'aws', as the current application is being hosted on an EC2 instance with an Amazon RDS PostgreSQL database. Currently, the restrictions on the database restrict massive deployment of the Docker image, however, the Dockerfile and R files (app.R and palettes.R) can easily be deployed in multiple EC2 instances within the same security group. (Not that there is a reason for that) However, the EC2 environment does give the ability to scale the size of the machine that the application is running on.

[Live Website](http://pipythagor.us/aqua_report)

## Aqua Report Data
The data being shown in the application graphs and data table are being collected from a Raspberry Pi in my home aquarium. Every 5 minutes, it INSERTs the average aquarium pH and temperature, and ambient Lux, into the Amazon RDS database supporting the application. This provides (near) real-time information about the fish tank. The app itself pings the database every 30 seconds for new data. The graphs and data table, if affected, are updated accordingly with reactive values.

[Raspberry Pi Data Collection Code](https://github.com/Coroecram/aquarium_pi)

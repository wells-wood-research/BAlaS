FROM tiangolo/uwsgi-nginx-flask:python3.6
# Setup BALS
WORKDIR /app
COPY ./dependencies_for_isambard/.isambard_settings /root/
COPY ./web/requirements.txt ./
RUN pip install -r ./requirements.txt
COPY ./web/instance/nginx.conf /etc/nginx/nginx.conf
COPY ./web/ /app/

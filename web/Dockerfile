FROM tiangolo/uwsgi-nginx-flask:python3.6
# Setup BALS
WORKDIR /app
COPY ./dependencies_for_isambard/.isambard_settings /root/
COPY ./web/requirements.txt ./
RUN pip install cython
RUN pip install -r ./requirements.txt
RUN rm /etc/nginx/conf.d/nginx.conf
COPY ./web/config/nginx.conf /etc/nginx/nginx.conf
COPY ./web/config/bals.conf /etc/nginx/conf.d
COPY ./web/ /app/

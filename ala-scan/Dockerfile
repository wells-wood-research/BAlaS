FROM python:3.6
COPY ./dependencies_for_isambard/ /dependencies_for_isambard/
COPY ./dependencies_for_isambard/.isambard_settings /root/
COPY ./ala-scan/requirements.txt ./
RUN pip install -r ./requirements.txt
COPY ./web/bals/database.py /app/
COPY ./ala-scan/ /app/
WORKDIR /app
ENTRYPOINT ["python", "job_manager.py"]

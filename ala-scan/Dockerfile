FROM python:3.6
COPY ./dependencies_for_isambard/ /dependencies_for_isambard/
COPY ./dependencies_for_isambard/.isambard_settings /root/
COPY ./ala-scan/requirements.txt ./
RUN pip install cython
RUN pip install -r ./requirements.txt
COPY ./ala-scan/alaScanApp-dist/ /app/alaScanApp-dist/
WORKDIR /app/alaScanApp-dist
RUN python ./setup_utils.py < setup_options.txt
COPY ./ala-scan/ /app/
COPY ./web/bals/database.py /app/
WORKDIR /app
ENTRYPOINT ["python", "job_manager.py"]

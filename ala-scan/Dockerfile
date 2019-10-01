FROM python:3.6
COPY ./dependencies_for_isambard/ /dependencies_for_isambard/
RUN ln -s /dependencies_for_isambard/scwrl/Scwrl4 /usr/local/bin/Scwrl4
COPY ./ala-scan/requirements.txt ./
RUN apt update && apt install -y libgl1-mesa-glx
RUN pip install cython
RUN pip install -r ./requirements.txt
COPY ./ala-scan/budeAlaScan-dist/ /app/budeAlaScan-dist/
WORKDIR /app/budeAlaScan-dist
RUN python ./setup_utils.py < setup_options.txt
COPY ./ala-scan/ /app/
COPY ./web/bals/database.py /app/
WORKDIR /app
ENTRYPOINT ["python", "job_manager.py"]

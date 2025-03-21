import os
import sys
import subprocess
import datetime

lst_msgtyp = [
  'ADPSFC', 'ADPUPA', 'AIRCAR', 'AIRCFT', 'ASCATW', 
  'ERS1DA', 'GOESND', 'GPSIPW', 'MSONET', 'PROFLR',
  'QKSWND', 'RASSDA', 'SATEMP', 'SATWND', 'SFCBOG',
  'SFCSHP', 'SPSSMI', 'SYNDAT', 'VADWND', 'WDSATR']


def check_dump():
    yr = int(sys.argv[2])

    time = datetime.datetime(yr,1,1,0)
    time_end = datetime.datetime(yr,12,31,18)
    dt = datetime.timedelta(hours=6)

    while time <= time_end:
        dir_this = 'out/dump/{:04d}/{}'\
                   .format(yr,time.strftime("%Y%m%d%H"))

        # Check log file
        path = os.path.join(dir_this,'log')
        if not os.path.isfile(path):
            print('File not found: {}'.format(path))
        else:
            sub = subprocess.run(['tail', '-n', '1', path], 
                       encoding='utf-8', stdout=subprocess.PIPE)
            is_ok = False
            if len(sub.stdout.splitlines()) >= 1:
                tail = sub.stdout.splitlines()[0].strip().split()
                if len(tail) > 3:
                    if tail[0] == '[-' and tail[1] == 'program' and tail[2] == 'main':
                        is_ok = True

            if not is_ok:
                print('Incomplete log file: {}'.format(path))

        # Check dump file
        #"""
        for msgtyp in lst_msgtyp:
            path = os.path.join(dir_this,'data.'+msgtyp)
            if not os.path.isfile(path):
                print('File not found: {}'.format(path))
            else:
                sub = subprocess.run(['tail', '-n', '1', path], 
                        encoding='utf-8', stdout=subprocess.PIPE)
                if len(sub.stdout.splitlines()) >= 1:
                    tail = sub.stdout.splitlines()[0].strip()
                    if tail != 'EOF':
                        print('Incomplete dump file: {}'.format(path))
        #"""

        time += dt


def check_thinning():
    runname = sys.argv[2]
    yr = int(sys.argv[3])

    time = datetime.datetime(yr,1,1,0)
    time_end = datetime.datetime(yr,12,31,18)
    dt = datetime.timedelta(hours=6)

    while time <= time_end:
        # Check log file
        path = 'out/thinning/{}/log/{}'\
               .format(runname,time.strftime("%Y%m%d%H"))
        if not os.path.isfile(path):
            print('File not found: {}'.format(path))
        else:
            sub = subprocess.run(['tail', '-n', '1', path],
                      encoding='utf-8', stdout=subprocess.PIPE)
            is_ok = False
            if len(sub.stdout.splitlines()) >= 1:
                tail = sub.stdout.splitlines()[0].strip().split()
                if len(tail) > 3:
                    if tail[0] == '[-' and tail[1] == 'program' and tail[2] == 'main':
                        is_ok = True
            if not is_ok:
                print('Incomplete log file: {}'.format(path))

        # Check data file
        path = 'out/thinning/{}/obs/{}.dat'\
               .format(runname,time.strftime("%Y%m%d%H"))
        if not os.path.isfile(path):
            print('File not found: {}'.format(path))

        time += dt


if __name__ == '__main__':
    job = sys.argv[1]

    if job == 'dump':
        check_dump()

    elif job == 'thinning':
        check_thinning()

    else:
        raise Exception('Invalid value in $job: '+job)


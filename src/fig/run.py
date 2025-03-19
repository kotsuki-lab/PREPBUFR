import os
import sys
import copy
import subprocess
import datetime
import numpy as np


def get_dymax(yr,mn):
    if mn == 2:
        dymax = 28
        if yr % 4 == 0:
            dymax = 29
            if yr % 100 == 0:
                dymax = 28
                if yr % 400 == 0:
                    dymax = 29
        return dymax
    elif mn in [4,6,9,11]:
        return 30
    elif mn in [1,3,5,7,8,10,12]:
        return 31
    else:
        raise Exception('Invalid value in $mn: {}'.format(mn))


def run(msgtyp, stime_s, stime_e):
    if stime_e[4:6] == '00':
        stime_e = stime_e[:4] + stime_s[4:6] + '0000'
    if stime_e[6:8] == '00':
        stime_e = stime_e[:6] + '{:02d}'.format(get_dymax(int(stime_e[:4]),int(stime_e[4:6])))\
                  + stime_e[8:10]
        if stime_e[8:10] == '00':
            stime_e = stime_e[:8] + '18'
    #print(stime_e)
    #return

    time_s = datetime.datetime(
               int(stime_s[:4]),int(stime_s[4:6]),int(stime_s[6:8]),int(stime_s[8:10]))
    time_e = datetime.datetime(
               int(stime_e[:4]),int(stime_e[4:6]),int(stime_e[6:8]),int(stime_e[8:10]))
    dt = datetime.timedelta(hours=6)

    time = time_s - dt
    while True:
        time += dt
        if time > time_e: break
        stime = time.strftime('%Y%m%d%H')
        print(stime)

        fin_data_template = '../../out/dump/{:04d}/{}/data'.format(time.year,stime)
        dirout_obs = '../../out/fig/{:04d}/{}'.format(time.year,stime)
        #dirout_obs = '../../tmp/fig/{:04d}/{}'.format(time.year,stime)
        os.makedirs(dirout_obs, exist_ok=True)
        subprocess.run(['srun', './main', fin_data_template, dirout_obs, msgtyp])


if __name__ == '__main__':
    args = sys.argv
    msgtyp = args[1]
    stime_s = args[2]
    stime_e = args[3]
    run(msgtyp, stime_s, stime_e)

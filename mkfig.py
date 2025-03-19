import os
import sys
import subprocess
import copy
import random
import datetime
import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import json
import PIL
import shapefile


nlon = 64
nlat = 32
nlev = 7
plv = [925, 850, 700, 600, 500, 250, 50]

PREPBUFR_miss = 1e11

dir_WeatherBench = '/data10/kotsuki/climax-letkf/weatherbench/v20240703/'

figtopdir = 'fig'


varname_long = dict(
  U  = 'Zonal Wind',
  V  = 'Meridional Wind',
  T  = 'Temperature',
  Q  = 'Specific Humidity',
  G  = 'Geopotential Height',
  Ps = 'Surface Pressure')


def get_lev(var):
    if var == 'u':
        lev0 = 0
        mlev = 7
    elif var == 'v':
        lev0 = 7
        mlev = 7
    elif var == 't':
        lev0 = 14
        mlev = 7
    elif var == 'q':
        lev0 = 21
        mlev = 7
    elif var == 'geo':
        lev0 = 28
        mlev = 7
    elif var == 'ps':
        lev0 = 38
        mlev = 1
    else:
        raise Exception('Invalid value in $var: {}'.format(var))

    return lev0, mlev


def get_unit(var):
    if var in ['U','V']:
        return 'm/s'
    elif var == 'T':
        return 'K'
    elif var == 'Q':
        return 'kg/kg'
    elif var == 'G':
        return r'$\mathrm{m^2/s^2}$'
    elif var == 'Ps':
        return 'hPa'
    else:
        raise Exception('Invalid value in $var: {}'.format(var))


def get_iz(var, lev):
    if var == 'U':
        iz = lev - 1
    elif var == 'V':
        iz = 7 + lev - 1
    elif var == 'T':
        iz = 14 + lev - 1
    elif var == 'Q':
        iz = 21 + lev - 1
    elif var == 'G':
        iz = 28 + lev - 1
    elif var == 'Ps':
        iz = 38
    else:
        raise Exception('Invalid value in $var: {}'.format(var))

    return iz


def str_to_bool(s):
    if s in ['T', 't', 'True', 'true', '.TRUE.', '.true.']:
        return True
    elif s in ['F', 'f', 'False', 'false', '.FALSE.', '.false.']:
        return False
    else:
        raise Exception('Invalid value in $s: {}'.format(s))


def get_dymax(yr,mn):
    if mn == 2:
        dymax = 28
        if yr % 4 == 0:
            dymax = 29
            if yr % 100 == 0:
                dymax = 28
                if yr % 400 == 0:
                    dymax = 29
    elif mn in [4,6,9,11]:
        dymax = 30
    else:
        dymax = 31
    return dymax


def get_grdwgt():
    latbnd = np.linspace(-np.pi/2,np.pi/2,nlat+1)[::-1]
    grdwgt = np.sin(latbnd[:-1]) - np.sin(latbnd[1:])
    grdwgt /= grdwgt.sum() * nlon
    grdwgt = np.tile(grdwgt.reshape(nlat,1),(1,nlon))
    #print(grdwgt.shape, grdwgt.sum())
    #plt.imshow(grdwgt, cmap=plt.cm.jet)
    #plt.show()

    return grdwgt


def read_rec_all(fname):
    def read_line(line):
        dat = line.strip().split()
        d = dict(
          lon = float(dat[0]),
          lat = float(dat[1]), 
          lev = float(dat[2]),
          iqmk = int(dat[3]),
          obs = float(dat[4]),
          oer = float(dat[5]),
          ilon_near = int(dat[6]), 
          ilat_near = int(dat[7]), 
          ilev_near = int(dat[8]), 
          ilon_blng = int(dat[9]), 
          ilat_blng = int(dat[10]), 
          ilev_blng = int(dat[11]), 
          wgtmax = float(dat[12]),
          is_valid = str_to_bool(dat[13]),
          is_selected = str_to_bool(dat[14])
        )
        return d

    return [read_line(line) for line in open(fname,'r').readlines()[1:]]


def add_margin_pole(dat):
    d = np.r_[np.full((1,nlon),np.nan),dat,np.full((1,nlon),np.nan)]
    latmax = 90.*(nlat+2)/nlat
    return d, latmax


def savefig(fig, figname):
    os.makedirs(os.path.dirname(figname), exist_ok=True)
    fig.savefig(figname, bbox_inches='tight', pad_inches=0.1, dpi=150)
    print('Saving '+figname)


def make_fig_global(fs=None, prj=ccrs.PlateCarree()):
    fig = plt.figure(figsize=fs)
    ax = fig.add_subplot(projection=prj)
    ax.set_global()
    ax.coastlines(linewidth=0.5)
    return fig, ax


def gen_cmap_rgb(cols):
    nmax = float(len(cols)-1)
    cdict = {'red':[], 'green':[], 'blue':[]}
    for n, c in enumerate(cols):
        loc = n/nmax
        cdict['red'  ].append((loc, c[0], c[0]))
        cdict['green'].append((loc, c[1], c[1]))
        cdict['blue' ].append((loc, c[2], c[2]))
    return mpl.colors.LinearSegmentedColormap('cmap', cdict)


def draw_fields_var():
    f_anal = args[2]
    var = args[3]

    lev0, mlev = get_lev(var)

    dirname = os.path.dirname(f_anal)
    filename = os.path.basename(f_anal)
    str_date = filename.strip().split('.')[0]
    figname = os.path.join(dirname,'{}_{}.png'.format(str_date, var))

    if mlev == 1:
        anal = np.fromfile(f_anal,dtype=np.float32).reshape(-1,nlat,nlon)[lev0].byteswap()
        fig = draw_field_single(anal)
    else:
        anal = np.fromfile(f_anal,dtype=np.float32).reshape(-1,nlat,nlon)[lev0:lev0+mlev].byteswap()
        fig = draw_field_multi(anal)

    print('Saving '+figname)
    plt.savefig(figname, bbox_inches='tight', pad_inches=0.1)
    plt.show()


def draw_field_single(dat):
    fig, ax = make_fig_global()
    im = ax.imshow(dat, cmap=plt.cm.jet, origin='lower', extent=[0,360,-90,90])
    fig.colorbar(im, orientation='horizontal')

    return fig


def draw_field_multi(anal, vmin=None, vmax=None, title=None):
    if type(anal) is list:
        m = len(anal)
    elif type(anal) is np.ndarray:
        m = anal.shape[0]
    else:
        raise Exception('Unknown type of anal: {}'.format(type(anal)))

    if vmin is None:
        vmin = np.min([anal[i].min() for i in range(m)])
    if vmax is None:
        vmax = np.max([anal[i].max() for i in range(m)])

    fig_width = 16
    fig_height = 3
    axes_left = 0.1
    axes_right = 0.9
    axes_top = 0.95
    axes_bottom = 0.1
    axes_width = fig_width * (axes_right - axes_left)
    axes_height = min(axes_width/m/2, fig_height*(axes_top-axes_bottom))
    cax_width = 0.4
    cax_height = 0.03
    cax_left = (1-cax_width) / 2
    cax_top = (fig_height - axes_height) * 0.5 / fig_height - cax_height

    grd_kw = dict(left=axes_left, right=axes_right, bottom=axes_bottom, top=axes_top,
                  wspace=0.05)
    cax_rect = (cax_left, cax_top, cax_width, cax_height)
    fig, ax = plt.subplots(1, m, figsize=(16,3), gridspec_kw=grd_kw,
                           subplot_kw=dict(projection=ccrs.PlateCarree()))
    for i in range(m):
        ax[i].set_global()
        ax[i].coastlines(linewidth=0.5)
        ax[i].imshow(anal[i], cmap=plt.cm.jet, origin='lower', 
                     extent=[0-180./nlon,360-180./nlon,-90,90],
                     vmin=vmin, vmax=vmax)
    if title is not None:
        for i in range(m):
            ax[i].set_title(title[i]+'\n[{:9.2e},{:9.2e}]'\
                            .format(anal[i].min(), anal[i].max()),
                            fontsize=10)
    else:
        for i in range(m):
            ax[i].set_title('[{:9.2e},{:9.2e}]'\
                            .format(anal[i].min(), anal[i].max()), 
                            fontsize=10)

    cax = fig.add_axes(cax_rect)
    norm = mpl.colors.Normalize(vmin=vmin, vmax=vmax)
    mappable = mpl.cm.ScalarMappable(cmap=plt.cm.jet, norm=norm)
    fig.colorbar(mappable, cax=cax, orientation='horizontal')

    return fig


def draw_fields():
    dir_run = args[2]
    var = args[3]
    lev = int(args[4])
    stime_s = args[5]
    stime_e = args[6]
    show = str_to_bool(args[7])

    vmax_ref  = None
    if var == 'U':
        ilev = lev - 1
        cm_ref = plt.cm.PRGn
        bounds_ref = [-25, -15, -10, -5, -2, 2, 5, 10, 15, 25]
        bounds_bias = [-6, -4, -2, -1, -0.5, 0.5, 1, 2, 4, 6]
        bounds_abbs = [0, 0.5, 1, 2, 4, 6, 10]
        bounds_bfrc = [-0.6, -0.4, -0.2, -0.1, 0.1, 0.2, 0.4, 0.6]
        bounds_rmse = [0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6]
        bounds_sprd = bounds_rmse
        unit = 'm/s'
    elif var == 'V':
        ilev = lev - 1
        cm_ref = plt.cm.PRGn
        bounds_ref = [-10, -8, -6, -4, -2, -1, 1, 2, 4, 6, 8, 10]
        bounds_bias = [-6, -4, -2, -1, -0.5, 0.5, 1, 2, 4, 6]
        bounds_abbs = [0, 0.5, 1, 2, 4, 6]
        bounds_bfrc = [-1.0, -0.8, -0.6, -0.4, -0.2, -0.1, 
                       0.1, 0.2, 0.4, 0.6, 0.8, 1.0]
        bounds_rmse = [0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6]
        bounds_sprd = bounds_rmse
        unit = 'm/s'
    elif var == 'T':
        ilev = lev - 1
        cm_ref = plt.cm.rainbow
        bounds_ref = np.arange(228,276,4)
        bounds_bias = [-2.5, -2, -1.5, -1, -0.5, 0.5, 1, 1.5, 2, 2.5]
        bounds_abbs = [0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4]
        bounds_bfrc = [-0.15, -0.12, -0.09, -0.06, -0.03, -0.01, 
                       0.01, 0.03, 0.06, 0.09, 0.12, 0.15]
        #bounds_rmse = [0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4]
        #bounds_sprd = [0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4]
        bounds_rmse = np.linspace(0,3,11)
        bounds_sprd = np.linspace(0,3,11)
        unit = 'K'
    elif var == 'Q':
        ilev = lev - 1
        unit = 'kg/kg'
        cmap = plt.cm.jet
    elif var == 'Geo':
        ilev = lev - 1
        cmap = plt.cm.jet
        unit = 'm'
    elif var == 'Ps':
        lev = ''
        ilev = 0
        cm_ref = plt.cm.viridis
        bounds_ref = [550, 600, 650, 700, 750, 800, 850, 900, 950, 1000, 1050]
        bounds_bias = [-12, -10, -8, -6, -4, -2, -1, 1, 2, 4, 6, 8, 10, 12]
        bounds_abbs = [0, 2, 4, 6, 8, 10, 12]
        bounds_bfrc = [-0.06, -0.04, -0.02, -0.01, 0.01, 0.02, 0.04, 0.06]
        bounds_rmse = [0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6]
        bounds_sprd = bounds_rmse
        unit = 'hPa'
    else:
        raise Exception('Invalid value in $var: {}'.format(var))

    #bounds_nobs = [0, 10, 100, 200, 300, 500, 700, 1000]
    bounds_nobs = [0, 10, 100, 200, 300, 500]

    iz = get_iz(var, lev)

    """cm_rmse = [
      #'#674598', # 青紫
      #'#460e44', # 紫紺
      #'#4c6cb3', # 群青色
      '#192f60', # 紺青
      '#1e50a2', # 瑠璃色
      '#0094c8', # 薄藍
      '#2ca9e1', # 天色
      '#a0d8ef', # 空色
      #'#a4d5bd', # スプレイグリーン
      '#98d98e', # 若緑
      '#d8e698', # 若菜色
      #'#f5e56b', # 苅安色
      #'#ffec47', # 菜の花色
      '#fef263', # 黄蘗色
      '#f8b862', # 萱草色
      '#ee7948', # 黄丹
      '#d9333f', # 紅赤
    ]"""
    #cm_rmse = [plt.cm.rainbow(ic) for ic in np.linspace(0,1,12)[:-1]]
    cm_rmse = plt.cm.rainbow
    cm_sprd = cm_rmse
    cm_nobs = plt.cm.Blues


    def make_cmap(cm, bounds, extend):
        if bounds is None:
            return cm, None
        ci = np.linspace(0,1,len(bounds)+1)

        if type(cm) is list:
            colorlist = cm
        elif type(cm) is mpl.colors.LinearSegmentedColormap or\
             type(cm) is mpl.colors.ListedColormap:
            colorlist = [cm(i) for i in ci]
        else:
            raise Exception('Invalid type in $type(cm): {}'.format(type(cm)))

        if extend == 'both':
            cmap = mpl.colors.ListedColormap(colorlist[1:-1])
            cmap.set_under(colorlist[0])
            cmap.set_over(colorlist[-1])
        elif extend == 'min':
            cmap = mpl.colors.ListedColormap(colorlist[1:])
            cmap.set_under(colorlist[0])
        elif extend == 'max':
            cmap = mpl.colors.ListedColormap(colorlist[:-1])
            cmap.set_over(colorlist[-1])
        elif extend == 'none':
            cmap = mpl.colors.ListedColormap(colorlist)

        norm = mpl.colors.BoundaryNorm(bounds, cmap.N)
        return cmap, norm

    
    cmap_ref, norm_ref = make_cmap(cm_ref, bounds_ref, 'both')
    cmap_bias, norm_bias = make_cmap(plt.cm.bwr, bounds_bias, 'both')
    cmap_abbs, norm_abbs = make_cmap(plt.cm.rainbow, bounds_abbs, 'max')
    cmap_bfrc, norm_bfrc = make_cmap(plt.cm.bwr, bounds_bfrc, 'both')
    #cmap_rmse, norm_rmse = make_cmap(cm_rmse, bounds_rmse, 'max')
    cmap_rmse, norm_rmse = cm_rmse, None
    #cmap_sprd, norm_sprd = make_cmap(cm_sprd, bounds_sprd, 'max')
    cmap_sprd, norm_sprd = cm_sprd, None
    #cmap_nobs, norm_nobs = make_cmap(cm_nobs, bounds_nobs, 'max')
    cmap_nobs, norm_nobs = cm_nobs, None

    fs = (8,6)
    prj = ccrs.PlateCarree(central_longitude=180)

    if stime_e[6:8] == '00':
        stime_e = stime_e[:6] + '{:02d}'.format(get_dymax(int(stime_e[:4]),int(stime_e[4:6])))\
                   + stime_e[8:]
        if stime_e[8:10] == '00':
            stime_e = stime_e[:8] + '18'

    dt = datetime.timedelta(hours=6)
    time_s = datetime.datetime(
               int(stime_s[:4]),int(stime_s[4:6]),int(stime_s[6:8]),int(stime_s[8:10]))
    time_e = datetime.datetime(
               int(stime_e[:4]),int(stime_e[4:6]),int(stime_e[6:8]),int(stime_e[8:10]))

    if dir_run[-1] == '/':
        dir_run = dir_run[:-1]
    runname = os.path.basename(dir_run)
    figdir = figtopdir+'/field/{}/{}-{}'.format(runname,stime_s,stime_e)
    os.makedirs(figdir,exist_ok=True)
    print('figdir: '+figdir)

    dir_data = os.path.basename(os.path.dirname(dir_run))
    dir_monit = 'out/thinning/'+dir_data.replace('prepbufr_','')+'/monit'
    if os.path.isdir(dir_monit):
        print('dir_monit: '+dir_monit)
    else:
        print('dir_monit not exist: '+dir_monit)
        dir_monit = ''


    datdir = os.path.join(figdir,'dat/{}')
    f_ref = os.path.join(datdir,'ref.bin')
    f_ges = os.path.join(datdir,'ges.bin')
    f_anl = os.path.join(datdir,'anl.bin')
    f_ges_bias = os.path.join(datdir,'ges_bias.bin')
    f_anl_bias = os.path.join(datdir,'anl_bias.bin')
    f_ges_abbs = os.path.join(datdir,'ges_abbs.bin')
    f_anl_abbs = os.path.join(datdir,'anl_abbs.bin')
    f_ges_rmse = os.path.join(datdir,'ges_rmse.bin')
    f_anl_rmse = os.path.join(datdir,'anl_rmse.bin')
    f_ges_sprd = os.path.join(datdir,'ges_sprd.bin')
    f_anl_sprd = os.path.join(datdir,'anl_sprd.bin')
    f_nobs = os.path.join(datdir,'nobs.bin')

    #"""
    ref = np.zeros((nlat,nlon))
    ges = np.zeros((nlat,nlon))
    anl = np.zeros((nlat,nlon))
    ges_bias = copy.deepcopy(anl)
    anl_bias = copy.deepcopy(anl)
    ges_abbs = copy.deepcopy(anl)
    anl_abbs = copy.deepcopy(anl)
    ges_rmse = copy.deepcopy(anl)
    anl_rmse = copy.deepcopy(anl)
    ges_sprd = copy.deepcopy(anl)
    anl_sprd = copy.deepcopy(anl)
    nobs = copy.deepcopy(anl)

    time = time_s - dt
    nt = 0
    while time < time_e:
        time += dt
        stime = time.strftime("%Y%m%d%H")
    
        f_ref_ = os.path.join(dir_WeatherBench,'{}.grd'.format(stime))
        f_ges_ = os.path.join(dir_run,'gues/mean/{}.grd'.format(stime))
        f_anl_ = os.path.join(dir_run,'anal/mean/{}.grd'.format(stime))
        f_ges_sprd_ = os.path.join(dir_run,'gues/sprd/{}.grd'.format(stime))
        f_anl_sprd_ = os.path.join(dir_run,'anal/sprd/{}.grd'.format(stime))
        f_nobs_ = os.path.join(dir_run,'nobs/{}.grd'.format(stime))
        ref_ = np.fromfile(f_ref_,dtype=np.float32).reshape(-1,nlat,nlon)[iz].byteswap()
        ges_ = np.fromfile(f_ges_,dtype=np.float32).reshape(-1,nlat,nlon)[iz].byteswap()
        anl_ = np.fromfile(f_anl_,dtype=np.float32).reshape(-1,nlat,nlon)[iz].byteswap()
        ges_sprd_ = np.fromfile(f_ges_sprd_,dtype=np.float32).reshape(-1,nlat,nlon)[iz].byteswap()
        anl_sprd_ = np.fromfile(f_anl_sprd_,dtype=np.float32).reshape(-1,nlat,nlon)[iz].byteswap()

        nobs += np.fromfile(f_nobs_,dtype=np.float32).reshape(-1,nlat,nlon)[iz].byteswap()

        if var == 'Q':
            ref_ *= 1e-6
            ges_ *= 1e-6
            anl_ *= 1e-6
            ges_sprd_ *= 1e-6
            anl_sprd_ *= 1e-6
        elif var == 'Ps':
            ref_ *= 1e-2
            ges_ *= 1e-2
            anl_ *= 1e-2
            ges_sprd_ *= 1e-2
            anl_sprd_ *= 1e-2

        nt += 1
        ges_bias_ = ges_ - ref_
        anl_bias_ = anl_ - ref_
        ref += ref_
        ges += ges_
        anl += anl_
        ges_bias += ges_bias_
        anl_bias += anl_bias_
        ges_abbs += np.abs(ges_bias_)
        anl_abbs += np.abs(anl_bias_)
        ges_rmse += ges_bias_**2
        anl_rmse += anl_bias_**2
        ges_sprd += ges_sprd_
        anl_sprd += anl_sprd_

    ref /= nt
    ges /= nt
    anl /= nt
    ges_bias /= nt
    anl_bias /= nt
    ges_abbs /= nt
    anl_abbs /= nt
    ges_rmse = np.sqrt(ges_rmse/nt)
    anl_rmse = np.sqrt(anl_rmse/nt)
    ges_sprd /= nt
    anl_sprd /= nt
    nobs /= nt

    os.makedirs(datdir,exist_ok=True)
    ref.astype(np.float32).tofile(f_ref)
    ges.astype(np.float32).tofile(f_ges)
    anl.astype(np.float32).tofile(f_anl)
    ges_bias.astype(np.float32).tofile(f_ges_bias)
    anl_bias.astype(np.float32).tofile(f_anl_bias)
    ges_abbs.astype(np.float32).tofile(f_ges_abbs)
    anl_abbs.astype(np.float32).tofile(f_anl_abbs)
    ges_rmse.astype(np.float32).tofile(f_ges_rmse)
    anl_rmse.astype(np.float32).tofile(f_anl_rmse)
    ges_sprd.astype(np.float32).tofile(f_ges_sprd)
    anl_sprd.astype(np.float32).tofile(f_anl_sprd)
    nobs.astype(np.float32).tofile(f_nobs)
    #return
    #"""

    ref = np.fromfile(f_ref,dtype=np.float32).reshape(nlat,nlon)
    ges = np.fromfile(f_ges,dtype=np.float32).reshape(nlat,nlon)
    anl = np.fromfile(f_anl,dtype=np.float32).reshape(nlat,nlon)
    ges_bias = np.fromfile(f_ges_bias,dtype=np.float32).reshape(nlat,nlon)
    anl_bias = np.fromfile(f_anl_bias,dtype=np.float32).reshape(nlat,nlon)
    ges_abbs = np.fromfile(f_ges_abbs,dtype=np.float32).reshape(nlat,nlon)
    anl_abbs = np.fromfile(f_anl_abbs,dtype=np.float32).reshape(nlat,nlon)
    ges_rmse = np.fromfile(f_ges_rmse,dtype=np.float32).reshape(nlat,nlon)
    anl_rmse = np.fromfile(f_anl_rmse,dtype=np.float32).reshape(nlat,nlon)
    ges_sprd = np.fromfile(f_ges_sprd,dtype=np.float32).reshape(nlat,nlon)
    anl_sprd = np.fromfile(f_anl_sprd,dtype=np.float32).reshape(nlat,nlon)
    nobs = np.fromfile(f_nobs,dtype=np.float32).reshape(nlat,nlon)

    print('RMSE ges: {:5.2f}, anl: {:5.2f}'.format(ges_rmse.mean(), anl_rmse.mean()))
    #iiy, iix = np.where(anl_bias==anl_bias.min())
    #for iy, ix in zip(iiy,iix):
    #    print('bias of anl is min {:.3f} @ ({},{}) anl={:.3f}, ref={:.3f}'.format(
    #          anl_bias.min(), iy, ix, anl[iy,ix], ref[iy,ix]))
    #iiy, iix = np.where(anl_bias==anl_bias.max())
    #for iy, ix in zip(iiy,iix):
    #    print('bias of anl is max {:.3f} @ ({},{}) anl={:.3f}, ref={:.3f}'.format(
    #          anl_bias.max(), iy, ix, anl[iy,ix], ref[iy,ix]))
    #return
    

    def imshow(ax, dat, cmap, norm, bounds):
        d, latmax = add_margin_pole(dat)
        if norm is None:
            return ax.imshow(d, cmap=cmap, vmin=bounds[0], vmax=bounds[-1], 
                             interpolation='nearest',
                             origin='lower', extent=[0,360,-latmax,latmax],
                             transform=ccrs.PlateCarree())
        else:
            return ax.imshow(d, cmap=cmap, norm=norm, 
                             interpolation='nearest',
                             origin='lower', extent=[0,360,-latmax,latmax],
                             transform=ccrs.PlateCarree())

    def set_ticks(bounds, unit=None):
        if bounds is not None:
            cb.set_ticks(bounds)
        if unit is not None:
            ax.text(180, -142, '[{}]'.format(unit), ha='right')


    # Ref.
    """
    fig0, ax = make_fig_global(fs, prj)
    im = imshow(ax, ref, cmap_ref, norm_ref)
    cb = fig0.colorbar(im, aspect=50, pad=0.08, orientation='horizontal', 
                       extend='both', extendfrac=0.1)
    set_ticks(bounds_ref, unit)
    ax.set_title('Ref, {}{}'.format(var,lev))
    savefig(fig0, os.path.join(figdir,'{}{}_ref.png'.format(var,lev)))
    #"""

    # Anal.
    """
    fig1, ax = make_fig_global(fs, prj)
    im = imshow(ax, anl, cmap_ref, norm_ref)
    cb = fig1.colorbar(im, aspect=50, pad=0.08, orientation='horizontal',
                       extend='both', extendfrac=0.1)
    set_ticks(bounds_ref, unit)
    ax.set_title('Anal, {}{}'.format(var,lev))
    figname = os.path.join(figdir,'{}{}_anl.png'.format(var,lev))
    print('Saving {}'.format(os.path.basename(figname)))
    fig1.savefig(figname, bbox_inches='tight', pad_inches=0.1, dpi=200)
    #"""

    # Bias
    """
    fig2, ax = make_fig_global(fs, prj)
    im = imshow(ax, ges_bias, cmap_bias, norm_bias)
    cb = fig2.colorbar(im, aspect=50, pad=0.08, orientation='horizontal',
                       extend='both', extendfrac=0.1)
    set_ticks(bounds_bias, unit)
    ax.set_title('Bias, ges, {}{}'.format(var,lev))
    figname = os.path.join(figdir,'{}{}_bias_ges.png'.format(var,lev))
    print('Saving {}'.format(os.path.basename(figname)))
    fig2.savefig(figname, bbox_inches='tight', pad_inches=0.1, dpi=200)
    #"""

    """
    fig3, ax = make_fig_global(fs, prj)
    im = imshow(ax, anl_bias, cmap_bias, norm_bias)
    cb = fig3.colorbar(im, aspect=50, pad=0.08, orientation='horizontal',
                       extend='both', extendfrac=0.1)
    set_ticks(bounds_bias, unit)
    ax.set_title('Bias, anl, {}{}'.format(var,lev))
    savefig(fig3, os.path.join(figdir,'{}{}_bias_anl.png'.format(var,lev)))
    #"""

    # Abs. bias
    """
    fig4, ax = make_fig_global(fs, prj)
    im = imshow(ax, ges_abbs, cmap_abbs, norm_abbs)
    fig4.colorbar(im, aspect=50, pad=0.08, orientation='horizontal',
                  extend='max', extendfrac=0.1)
    set_ticks(bounds_abbs, unit)
    ax.set_title('Abs. of bias, ges, {}{}'.format(var,lev))
    figname = os.path.join(figdir,'{}{}_abbs_ges.png'.format(var,lev))
    print('Saving {}'.format(os.path.basename(figname)))
    fig4.savefig(figname, bbox_inches='tight', pad_inches=0.1, dpi=200)
    """

    """
    fig5, ax = make_fig_global(fs, prj)
    im = imshow(ax, anl_abbs, cmap_abbs, norm_abbs)
    cb = fig5.colorbar(im, aspect=50, pad=0.08, orientation='horizontal',
                       extend='max', extendfrac=0.1)
    set_ticks(bounds_abbs, unit)
    ax.set_title('abs. of bias, anl, {}{}'.format(var,lev))
    savefig(fig5, os.path.join(figdir,'{}{}_abbs_anl.png'.format(var,lev)))
    #"""

    # Inc. of abs. of bias
    vmax = np.abs(anl_abbs-ges_abbs).max()*0.9

    """
    fig6, ax = make_fig_global(fs, prj)
    im = ax.imshow(anl_abbs-ges_abbs, cmap=plt.cm.PRGn_r, vmin=-vmax, vmax=vmax,
                   origin='lower', extent=[0,360,-90,90])
    fig6.colorbar(im, aspect=50, pad=0.08, orientation='horizontal',
                  extend='both', extendfrac=0.1)
    set_ticks(bounds_abbsinc, unit)
    ax.set_title('Inc. of abs. of bias, {}{}'.format(var,lev))
    figname = os.path.join(figdir,'{}{}_abbsinc.png'.format(var,lev))
    print('Saving {}'.format(os.path.basename(figname)))
    fig6.savefig(figname, bbox_inches='tight', pad_inches=0.1, dpi=200)
    """

    # Fraction of bias
    anl1 = anl.reshape(-1)
    arg = np.argsort(anl1)
    anl1 = anl1[arg]
    brng = anl1[int(anl1.size*0.9)] - anl1[int(anl1.size*0.1)]
    anl_bfrc = anl_bias / brng
    """
    fig7, ax = make_fig_global(fs, prj)
    im = imshow(ax, anl_bfrc, cmap_bfrc, norm_bfrc)
    cb = fig7.colorbar(im, aspect=50, pad=0.08, orientation='horizontal',
                       extend='both', extendfrac=0.1)
    set_ticks(bounds_bfrc)
    ax.set_title('Fraction of bias, anl, {}{}'.format(var,lev))
    #"""

    """
    fig8, ax = make_fig_global(fs, prj)
    im = imshow(ax, anl_bfrc, cmap_bfrc, norm_bfrc)
    cb = fig8.colorbar(im, aspect=50, pad=0.08, orientation='horizontal',
                       extend='both', extendfrac=0.1)
    set_ticks(bounds_bfrc)
    ax.set_title('Fraction of bias, anl, {}{}'.format(var,lev))
    #"""

    # RMSE
    #"""
    fig9, ax = make_fig_global(fs, prj)
    im = imshow(ax, ges_rmse, cmap_rmse, norm_rmse, bounds_rmse)
    cb = fig9.colorbar(im, aspect=50, pad=0.04, orientation='horizontal',
                       extend='max', extendfrac=0.1)
    #set_ticks(bounds_rmse)
    #ax.set_title('RMSE, ges, {}{}'.format(var,lev))
    savefig(fig9, os.path.join(figdir,'{}{}_rmse_ges.png'.format(var,lev)))
    #"""

    """
    fig10, ax = make_fig_global(fs, prj)
    im = imshow(ax, anl_rmse, cmap_rmse, norm_rmse)
    cb = fig10.colorbar(im, aspect=50, pad=0.04, orientation='horizontal',
                        extend='max', extendfrac=0.1)
    set_ticks(bounds_rmse)
    #ax.set_title('RMSE, anl, {}{}'.format(var,lev))
    #savefig(fig10, os.path.join(figdir,'{}{}_rmse_anl.png'.format(var,lev)))
    #"""

    # Spread
    #"""
    fig11, ax = make_fig_global(fs, prj)
    im = imshow(ax, ges_sprd, cmap_sprd, norm_sprd, bounds_sprd)
    cb = fig11.colorbar(im, aspect=50, pad=0.04, orientation='horizontal',
                        extend='max', extendfrac=0.1)
    #set_ticks(bounds_sprd)
    #ax.set_title('Spread, ges, {}{}'.format(var,lev))
    savefig(fig11, os.path.join(figdir,'{}{}_sprd_ges.png'.format(var,lev)))
    #"""

    """
    fig12, ax = make_fig_global(fs, prj)
    im = imshow(ax, anl_sprd, cmap_sprd, norm_sprd)
    cb = fig12.colorbar(im, aspect=50, pad=0.04, orientation='horizontal',
                        extend='max', extendfrac=0.1)
    set_ticks(bounds_sprd)
    #ax.set_title('Spread, anl, {}{}'.format(var,lev))
    #savefig(fig12, os.path.join(figdir,'{}{}_sprd_anl.png'.format(var,lev)))
    #"""

    # RMSE vs. Spread
    """
    vmax = max(anl_rmse.max(), anl_sprd.max())*1.05

    fig13, ax = plt.subplots(figsize=(6,6), 
                             gridspec_kw=dict(left=0.15, bottom=0.15, right=0.9, top=0.9))
    ax.scatter(anl_rmse, anl_sprd, s=10, edgecolor='none', facecolor='dimgray')
    ax.plot([0,vmax], [0,vmax], linewidth=0.5, color='k')
    ax.set_xlim(0,vmax)
    ax.set_ylim(0,vmax)
    ax.set_xlabel('RMSE')
    ax.set_ylabel('Spread')
    ax.set_title('anl, {}{}'.format(var, lev))
    savefig(fig13, os.path.join(figdir,'{}{}_rmse_vs_sprd.png'.format(var,lev)))
    """

    # nobs
    """
    fig13, ax = make_fig_global(fs, prj)
    im = imshow(ax, nobs, cmap_nobs, norm_nobs, bounds_nobs)
    cb = fig13.colorbar(im, aspect=50, pad=0.04, orientation='horizontal',
                        extend='max', extendfrac=0.1)
    #set_ticks(bounds_nobs)
    #savefig(fig13, os.path.join(figdir,'{}{}_nobs.png'.format(var,lev)))
    #"""

    # RMSE vs nobs
    """
    lons = np.linspace(0,360,nlon+1)[:-1]+180./nlon
    lats = np.linspace(-90+90./nlat,90-90./nlat,nlat)
    lons, lats = np.meshgrid(lons,lats)

    fig14, ax = plt.subplots(figsize=(6,6), 
                             gridspec_kw=dict(left=0.15, bottom=0.15, right=0.9, top=0.9))
    #im = ax.scatter(ges_rmse, nobs, c=lats, s=10, edgecolor='none',
    #                cmap=plt.cm.Spectral, 
    #                vmin=-90, vmax=90)
    #fig14.colorbar(im, aspect=50, pad=0.08, orientation='horizontal')

    bounds = [-50,-20,20,50]
    colors = ['tomato', 'gold', 'mediumseagreen', 'dodgerblue', 'darkorchid']
    for i, (vmin, vmax) in enumerate(zip([-90]+bounds,bounds+[90])):
        mask = (vmin<lats) & (lats<vmax)
        ax.scatter(ges_rmse[mask], nobs[mask],
                   s=10, edgecolor='none', facecolor=colors[i])
    """

    if show:
        plt.show()

    #plt.close(fig0)
    #plt.close(fig1)
    #plt.close(fig2)
    #plt.close(fig3)
    #plt.close(fig4)
    #plt.close(fig5)
    #plt.close(fig6)
    #plt.close(fig7)



def draw_timeseries_stats():
    dir_DATA = args[2]
    var = args[3]
    lev = int(args[4])

    if var == 'U':
        ylim = (2.4,5.1)
        yticks = np.arange(2.5,5.5,0.5)
        yticklabels = ['{:.1f}'.format(y) for y in yticks]
    elif var == 'V':
        ylim = (2.4,5.1)
        yticks = np.arange(2.5,5.5,0.5)
        yticklabels = ['{:.1f}'.format(y) for y in yticks]
    elif var == 'T':
        ylim = (0.7,2.6)
        yticks = np.arange(0.9,2.7,0.3)
        yticklabels = ['{:.1f}'.format(y) for y in yticks]
    elif var == 'Q':
        pass
    elif var == 'G':
        ylim = (150,750)
        yticks = np.arange(100,800,100)
        yticklabels = yticks
    elif var == 'Ps':
        ylim = (190,510)
        yticks = np.arange(200,550,50)
        yticklabels = yticks
    else:
        raise Exception('Invalid value in $var: {}'.format(var))

    if var == 'Ps':
        slev =''
    else:
        slev = str(lev)

    stime_bgn = '2016010100'
    stime_end = '2017123118'

    iz = get_iz(var,lev)
    unit = get_unit(var)

    grdwgt = get_grdwgt()


    # Get xticks
    xticks, xticklabels = [], []
    time_bgn = datetime.datetime(int(stime_bgn[:4]),int(stime_bgn[4:6]),
                                 int(stime_bgn[6:8]),int(stime_bgn[8:10]))
    time_end = datetime.datetime(int(stime_end[:4]),int(stime_end[4:6]),
                                 int(stime_end[6:8]),int(stime_end[8:10]))
    dt = datetime.timedelta(hours=6)
    time = time_bgn - dt
    i = -1
    while time < time_end:
        time += dt
        i += 1
        if time.month == 1 and time.day == 1 and time.hour == 0:
            xticks.append(i)
            xticklabels.append('{:04d}/{:02d}/{:02d}'.format(time.year,time.month,time.day))
        elif time.month in [4,7,10] and time.day == 1 and time.hour == 0:
            xticks.append(i)
            xticklabels.append('{:02d}/{:02d}'.format(time.month,time.day))


    def get_runname(dir_run):
        return dir_run[dir_run.index('CLIMAX'):].replace('/','')


    def read_stats(dir_run,step):
        runname = get_runname(dir_run)
        #print(runname)
        f_rmse = os.path.join(dir_run,'out/{}_rmse_time_prepbufr_{}_glb.txt'.format(step,runname))
        f_sprd = os.path.join(dir_run,'out/{}_sprd_time_prepbufr_{}_glb.txt'.format(step,runname))

        rmse = [float(line.strip().split()[2:][iz]) for line in open(f_rmse,'r').readlines()]
        sprd = [float(line.strip().split()[2:][iz]) for line in open(f_sprd,'r').readlines()]

        return np.array(rmse), np.array(sprd)


    def get_sprd_from_bin(dir_DATA, runname, step):
        f = figtopdir+'/timeseries_stats/dat/{}/{}_sprd_{}{}_{}-{}.bin'\
            .format(runname,step,var,slev,stime_bgn,stime_end)

        if os.path.isfile(f):
            sprd = np.fromfile(f,dtype=np.float32)
        else:
            print('Calculating global mean of Spread.')
            sprd = []
            time = time_bgn - dt
            while time < time_end:
                time += dt
                f_sprd = os.path.join(dir_DATA,'{}/{}/sprd/{}.grd'.format(
                                      runname,step,time.strftime('%Y%m%d%H')))
                sprd.append((np.fromfile(f_sprd,dtype=np.float32)\
                             .reshape(-1,nlat,nlon)[iz].byteswap()**2*grdwgt).sum())
            sprd = np.sqrt(np.array(sprd))

            os.makedirs(os.path.dirname(f),exist_ok=True)
            sprd.astype(np.float32).tofile(f)

        return sprd


    def get_rmse_from_bin(dir_DATA, runname, step):
        f = figtopdir+'/timeseries_stats/dat/{}/{}_rmse_{}{}_{}-{}.bin'\
            .format(runname,step,var,slev,stime_bgn,stime_end)

        if os.path.isfile(f):
            rmse = np.fromfile(f,dtype=np.float32)
        else:
            print('Calculating global mean of RMSE.')
            rmse = []
            time = time_bgn - dt
            while time < time_end:
                time += dt
                f_gues = os.path.join(dir_DATA,'{}/{}/mean/{}.grd'.format(
                                      runname,step,time.strftime('%Y%m%d%H')))
                f_base = os.path.join(dir_DATA,'../weatherbc/{}.grd'.format(
                                      time.strftime('%Y%m%d%H')))
                gues = np.fromfile(f_gues,dtype=np.float32).reshape(-1,nlat,nlon)[iz].byteswap()
                base = np.fromfile(f_base,dtype=np.float32).reshape(-1,nlat,nlon)[iz].byteswap()
                rmse.append(((gues-base)**2*grdwgt).sum())
            rmse = np.sqrt(np.array(rmse))

            os.makedirs(os.path.dirname(f),exist_ok=True)
            rmse.astype(np.float32).tofile(f)

        return rmse


    def calc_mav(dat, mt):
        new = [np.nan]*mt + [dat[it-mt:it+mt].mean() for it in range(mt,nt-mt)] + [np.nan]*mt

        return np.array(new)


    methods = ['RTPS','RTPP']
    lst_factor = dict(
        RTPS = [0.9 ,1.0 ,1.1 ,1.2 ,1.3 ],
        RTPP = [0.80,0.85,0.90,0.95,1.00],
    )
    fmt_factor = dict(
        RTPS = '3.1f',
        RTPP = '4.2f',
    )
    factor_best = dict(RTPS=1.20, RTPP=0.90)

    if var == 'Ps':
        title = '{{}} of {} [{}]'.format(varname_long[var], unit)
    else:
        title = '{{}} of {} at {} hPa [{}]'.format(varname_long[var], plv[lev-1], unit)

    # Draw each of RTPS and RTPP
    fs = (8,4)
    left, right, bottom, top = 0.10, 0.95, 0.15, 0.90
    lw = 0.8
    colors = ['mediumorchid','dodgerblue','mediumseagreen','goldenrod','tomato']
    fontsize_legend = 10

    #for step in ['gues', 'anal']:
    for step in ['gues']:
        if step == 'gues':
            dname = 'Background'
        elif step == 'anal':
            dname = 'Analysis'

        rmse_mav_best, sprd_mav_best = {}, {}

        mavrange = 12
        for method in methods:
            print('method: {}'.format(method))
            rmse, sprd = {}, {}
            rmse_mav, sprd_mav = {}, {}
            for factor in lst_factor[method]:
                print('factor: {}'.format(factor))
                runname = 'V250122_CLIMAXint-0LETKF_M000020L0600_inf1.00_{}{:4.2f}_ad0.00_po0.00'\
                          .format(method,factor)
                dir_run = os.path.join(dir_DATA,runname)
                #rmse[factor], sprd[factor] = read_stats(dir_run,step)
                rmse[factor] = get_rmse_from_bin(dir_DATA,runname,step)
                sprd[factor] = get_sprd_from_bin(dir_DATA,runname,step)

                nt = len(rmse[factor])
                rmse_mav[factor] = calc_mav(rmse[factor], mavrange)
                sprd_mav[factor] = calc_mav(sprd[factor], mavrange)

            rmse_mav_best[method] = copy.deepcopy(rmse_mav[factor_best[method]])
            sprd_mav_best[method] = copy.deepcopy(sprd_mav[factor_best[method]])

            fig = plt.figure(figsize=fs)
            ax = fig.add_axes([left,bottom,right-left,top-bottom])
            ax2 = ax.twinx()
            for i, factor in enumerate(lst_factor[method]):
                fmt_label = r'$\alpha={{:{}}}$'.format(fmt_factor[method])
                ax.plot(rmse_mav[factor], color=colors[i], linewidth=lw, 
                        label=fmt_label.format(factor))
                ax.plot(sprd_mav[factor], color=colors[i], linewidth=lw*0.5, linestyle='dashed')
            ax.tick_params(direction='in')
            ax.set_xticks(xticks,xticklabels)
            ax.set_xlim(0,nt)
            ax.set_yticks(yticks,yticklabels)
            ax.set_ylim(*ylim)
            ax2.tick_params(axis='y', direction='in', length=plt.rcParams['ytick.major.size'])
            ax2.set_yticks(yticks,[])
            ax2.set_ylim(*ylim)
            ax.set_xlabel('date')
            ax.set_ylabel('RMSE, Spread [{}]'.format(unit))
            ax.legend(loc='upper left', title=method,
                      fontsize=fontsize_legend, labelspacing=fontsize_legend*0.03)
            #ax.set_title(title.format(dname))
            savefig(fig, figtopdir+'/timeseries_stats/{}/{}_{}{}.png'.format(method,step,var,slev))

        # Draw RTPS vs. RTPP
        color = dict(RTPS='dodgerblue', RTPP='tomato')

        fig = plt.figure(figsize=fs)
        ax = fig.add_axes([left,bottom,right-left,top-bottom])
        ax2 = ax.twinx()
        for method in methods:
            ax.plot(rmse_mav_best[method], color=color[method], linewidth=lw, 
                    label=r'{} ($\alpha={:.2f}$)'.format(method, factor_best[method]))
            ax.plot(sprd_mav_best[method], color=color[method], linewidth=lw*0.5, linestyle='dashed')
        ax.tick_params(direction='in')
        ax.set_xticks(xticks,xticklabels)
        ax.set_xlim(0,nt)
        ax.set_yticks(yticks,yticklabels)
        ax.set_ylim(*ylim)
        ax2.tick_params(axis='y', direction='in', length=plt.rcParams['ytick.major.size'])
        ax2.set_yticks(yticks,[])
        ax2.set_ylim(*ylim)
        ax.set_xlabel('date')
        ax.set_ylabel('RMSE, Spread [{}]'.format(unit))
        ax.legend(loc='upper left', 
                  fontsize=fontsize_legend, labelspacing=fontsize_legend*0.03)
        #ax.set_title(title.format(dname))
        savefig(fig, figtopdir+'/timeseries_stats/RTPS_vs_RTPP/{}_{}{}.png'.format(step,var,slev))

        plt.show()


def draw_vplot_diff():
    dirname = args[2]
    year = int(args[3])
    mnbgn = int(args[4])
    mnend = int(args[5])

    figdir = os.path.join(dir_run,'fig_vplot_diff')
    os.makedirs(figdir,exist_ok=True)

    dir_stats = os.path.join(dir_run,'stats_vplot_diff')
    os.makedirs(dir_stats,exist_ok=True)

    if mnbgn == 1 and mnend == 12:
        span = '{:04d}all'.format(year)
    else:
        span = '{:04d}{:02d}-{:02d}'.format(year,mnbgn,mnend)

    def get_bias(dat, ref, l0, l1):
        return (dat[l0:l1] - ref[l0:l1]).mean(axis=(1,2))

    def get_rmse(dat, ref, l0, l1):
        return np.sqrt(((dat[l0:l1] - ref[l0:l1])**2).mean(axis=(1,2)))

    def timestep(yr,mn,dy,hr):
        version = 1

        time = '{:04d}{:02d}{:02d}{:02d}'.format(yr,mn,dy,hr)
        f_stats = os.path.join(dir_stats,'{}.txt'.format(time))
        file_exists = False
        if os.path.isfile(f_stats):
            lines = open(f_stats,'r').readlines()
            if len(lines) > 0:
                if lines[0].strip() != '':
                    if lines[0].strip().split()[0] == 'version':
                        try:
                            if int(lines[0].strip().split()[1]) == version:
                                file_exists = True
                        except TypeError:
                            pass

        if file_exists:
            dat = []
            for line in lines[1:]:
                dat.append(np.array([float(v) for v in line.strip().split()]))

        else:
            f_anl = os.path.join(dir_run,'anal/mean/{}.grd'.format(time))
            f_ges = os.path.join(dir_run,'gues/mean/{}.grd'.format(time))
            f_ref = '../DATA/weatherbc/{}.grd'.format(time)
            anl = np.fromfile(f_anl,dtype=np.float32).byteswap().reshape(-1,nlat,nlon)
            ges = np.fromfile(f_ges,dtype=np.float32).byteswap().reshape(-1,nlat,nlon)
            ref = np.fromfile(f_ref,dtype=np.float32).byteswap().reshape(-1,nlat,nlon)

            dat = [
              get_bias(anl, ref,  0,  7), # U
              get_bias(ges, ref,  0,  7),
              get_rmse(anl, ref,  0,  7),
              get_rmse(ges, ref,  0,  7),
              get_bias(anl, ref,  7, 14), # V
              get_bias(ges, ref,  7, 14),
              get_rmse(anl, ref,  7, 14),
              get_rmse(ges, ref,  7, 14),
              get_bias(anl, ref, 14, 21), # T
              get_bias(ges, ref, 14, 21),
              get_rmse(anl, ref, 14, 21),
              get_rmse(ges, ref, 14, 21),
              get_bias(anl, ref, 21, 28), # Q
              get_bias(ges, ref, 21, 28),
              get_rmse(anl, ref, 21, 28),
              get_rmse(ges, ref, 21, 28),
              get_bias(anl, ref, 28, 35), # G
              get_bias(ges, ref, 28, 35),
              get_rmse(anl, ref, 28, 35),
              get_rmse(ges, ref, 28, 35),
              get_bias(anl, ref, 35, 36), # Ps
              get_bias(ges, ref, 35, 36),
              get_rmse(anl, ref, 35, 36),
              get_rmse(ges, ref, 35, 36),
            ]
            print('Writing {}'.format(f_stats))
            with open(f_stats,'w') as fp:
                fp.write('version {}\n'.format(version))
                # U, V, T, Q, G  (7 layers * 4 stats * 5 vars)
                for i in range(0,20):
                    fp.write((' {:9.2e}'*7+'\n').format(*tuple(dat[i])))
                # Ps (1 layer * 4 stats * 1 var)
                for i in range(20,24):
                    fp.write((' {:9.2e}'*1+'\n').format(*tuple(dat[i])))

        U_bias_anl.append(dat[0])
        U_bias_ges.append(dat[1])
        U_rmse_anl.append(dat[2])
        U_rmse_ges.append(dat[3])
        V_bias_anl.append(dat[4])
        V_bias_ges.append(dat[5])
        V_rmse_anl.append(dat[6])
        V_rmse_ges.append(dat[7])
        T_bias_anl.append(dat[8])
        T_bias_ges.append(dat[9])
        T_rmse_anl.append(dat[10])
        T_rmse_ges.append(dat[11])
        Q_bias_anl.append(dat[12])
        Q_bias_ges.append(dat[13])
        Q_rmse_anl.append(dat[14])
        Q_rmse_ges.append(dat[15])
        G_bias_anl.append(dat[16])
        G_bias_ges.append(dat[17])
        G_rmse_anl.append(dat[18])
        G_rmse_ges.append(dat[19])
        Ps_bias_anl.append(dat[20])
        Ps_bias_ges.append(dat[21])
        Ps_rmse_anl.append(dat[22])
        Ps_rmse_ges.append(dat[23])


    U_bias_anl , U_bias_ges , U_rmse_anl , U_rmse_ges  = [], [], [], []
    V_bias_anl , V_bias_ges , V_rmse_anl , V_rmse_ges  = [], [], [], []
    T_bias_anl , T_bias_ges , T_rmse_anl , T_rmse_ges  = [], [], [], []
    Q_bias_anl , Q_bias_ges , Q_rmse_anl , Q_rmse_ges  = [], [], [], []
    G_bias_anl , G_bias_ges , G_rmse_anl , G_rmse_ges  = [], [], [], []
    Ps_bias_anl, Ps_bias_ges, Ps_rmse_anl, Ps_rmse_ges = [], [], [], []
    for mn in range(mnbgn,mnend+1):
        print('Reading {:2d}...'.format(mn))
        fname = os.path.join(figdir,'vplot_{:02d}.bin'.format(mn))
        for dy in range(1,get_dymax(year,mn)+1):
            for hr in range(0,24,6):
                timestep(year,mn,dy,hr)
    U_bias_anl  = np.array(U_bias_anl).mean(axis=0)
    U_bias_ges  = np.array(U_bias_ges).mean(axis=0)
    U_rmse_anl  = np.array(U_rmse_anl).mean(axis=0)
    U_rmse_ges  = np.array(U_rmse_ges).mean(axis=0)
    V_bias_anl  = np.array(V_bias_anl).mean(axis=0)
    V_bias_ges  = np.array(V_bias_ges).mean(axis=0)
    V_rmse_anl  = np.array(V_rmse_anl).mean(axis=0)
    V_rmse_ges  = np.array(V_rmse_ges).mean(axis=0)
    T_bias_anl  = np.array(T_bias_anl).mean(axis=0)
    T_bias_ges  = np.array(T_bias_ges).mean(axis=0)
    T_rmse_anl  = np.array(T_rmse_anl).mean(axis=0)
    T_rmse_ges  = np.array(T_rmse_ges).mean(axis=0)
    Q_bias_anl  = np.array(Q_bias_anl).mean(axis=0)
    Q_bias_ges  = np.array(Q_bias_ges).mean(axis=0)
    Q_rmse_anl  = np.array(Q_rmse_anl).mean(axis=0)
    Q_rmse_ges  = np.array(Q_rmse_ges).mean(axis=0)
    G_bias_anl  = np.array(G_bias_anl).mean(axis=0)
    G_bias_ges  = np.array(G_bias_ges).mean(axis=0)
    G_rmse_anl  = np.array(G_rmse_anl).mean(axis=0)
    G_rmse_ges  = np.array(G_rmse_ges).mean(axis=0)
    Ps_bias_anl = np.array(Ps_bias_anl).mean(axis=0)
    Ps_bias_ges = np.array(Ps_bias_ges).mean(axis=0)
    Ps_rmse_anl = np.array(Ps_rmse_anl).mean(axis=0)
    Ps_rmse_ges = np.array(Ps_rmse_ges).mean(axis=0)



    def draw(bias_anl, bias_ges, rmse_anl, rmse_ges, varname, unit):
        fs = (5,6)
        grd_kw = dict(left=0.2, right=0.95, bottom=0.1, top=0.95,
                      wspace=0.1) 
        x_title = 0.2 + (0.95 - 0.2)*0.5
        ymin, ymax = 0, nlev-1
        xmax = max(np.abs(bias_anl).max(), np.abs(bias_ges).max()) * 1.05
        fig, ax = plt.subplots(1, 2, figsize=fs, gridspec_kw=grd_kw)

        ax[0].vlines([0], ymin, ymax, linewidth=0.5, color='k')
        ax[0].plot(bias_anl, range(nlev), color='dimgray', label='anal')
        ax[0].plot(bias_ges, range(nlev), color='royalblue', label='gues')
        ax[0].set_yticks(range(nlev), plv)
        ax[0].set_xlim(-xmax, xmax)
        ax[0].set_ylim(ymin, ymax)
        ax[0].set_ylabel('Pressure [hPa]')
        ax[0].set_xlabel('Bias')

        ax[1].vlines([0], ymin, ymax, linewidth=0.5, color='k')
        ax[1].plot(rmse_anl, range(nlev), color='dimgray', label='anal')
        ax[1].plot(rmse_ges, range(nlev), color='royalblue', label='gues')
        ax[1].set_yticks(range(nlev), [])
        ax[1].set_xlim(0, None)
        ax[1].set_ylim(ymin, ymax)
        ax[1].set_xlabel('RMSE')

        fig.suptitle('{} [{}]'.format(varname,unit), ha='center', x=x_title)

        figname = os.path.join(figdir,'{}_{}.png'.format(varname,span))
        savefig(fig, figname)

        return fig


    def draw_sfc(bias_anl, bias_ges, rmse_anl, rmse_ges, varname, unit):
        fs = (5,1.5)
        grd_kw = dict(left=0.2, right=0.95, bottom=0.4, top=0.8,
                      wspace=0.1) 
        x_title = 0.2 + (0.95 - 0.2)*0.5
        ymin, ymax = -0.5, 0.5
        xmax = max(np.abs(bias_anl), np.abs(bias_ges)) * 1.05
        sz = 15
        fig, ax = plt.subplots(1, 2, figsize=fs, gridspec_kw=grd_kw)

        ax[0].vlines([0], ymin, ymax, linewidth=0.5, color='k')
        ax[0].scatter(bias_anl, 0, s=sz, marker='o', facecolor='dimgray', 
                      edgecolor='k', linewidth=0.5, label='anal')
        ax[0].scatter(bias_ges, 0, s=sz, marker='^', facecolor='royalblue', 
                      edgecolor='k', linewidth=0.5, label='gues')
        ax[0].set_yticks([ymin,0,ymax], ['', 'surface', ''], rotation=90, va='center')
        ax[0].set_xlim(-xmax, xmax)
        ax[0].set_ylim(ymin, ymax)
        ax[0].set_xlabel('Bias')

        ax[1].vlines([0], ymin, ymax, linewidth=0.5, color='k')
        ax[1].scatter(rmse_anl, 0, s=sz, marker='o', facecolor='dimgray', 
                      edgecolor='k', linewidth=0.5, label='anal')
        ax[1].scatter(rmse_ges, 0, s=sz, marker='^', facecolor='royalblue', 
                      edgecolor='k', linewidth=0.5, label='gues')
        ax[1].set_yticks([ymin,0,ymax], [])
        ax[1].set_xlim(0, None)
        ax[1].set_ylim(ymin, ymax)
        ax[1].set_xlabel('RMSE')

        fig.suptitle('{} [{}]'.format(varname,unit), ha='center', x=x_title)

        figname = os.path.join(figdir,'{}_{}.png'.format(varname,span))
        savefig(fig, figname)

        return fig

    #fig1 = draw(U_bias_anl , U_bias_ges , U_rmse_anl , U_rmse_ges , 'U', 'm/s')
    #fig2 = draw(V_bias_anl , V_bias_ges , V_rmse_anl , V_rmse_ges , 'V', 'm/s')
    #fig3 = draw(T_bias_anl , T_bias_ges , T_rmse_anl , T_rmse_ges , 'T', 'K')
    #fig4 = draw(Q_bias_anl , Q_bias_ges , Q_rmse_anl , Q_rmse_ges , 'Q', 'kg/kg')
    #fig5 = draw(G_bias_anl , G_bias_ges , G_rmse_anl , G_rmse_ges , 'G', 'm')
    fig6 = draw_sfc(Ps_bias_anl, Ps_bias_ges, Ps_rmse_anl, Ps_rmse_ges, 'Ps', 'hPa')

    plt.show()

    #plt.close(fig1)
    #plt.close(fig2)
    #plt.close(fig3)
    #plt.close(fig4)
    #plt.close(fig5)
    plt.close(fig6)


def draw_timeprogress_fcstexp():
    dir_ClimaX = sys.argv[2]
    var = sys.argv[3]
    lev = int(sys.argv[4])

    alpha = dict(RTPS=1.20, RTPP=0.90)

    stime_bgn = '2017010100'
    stime_end = '2017013118'
    figname = figtopdir+'/timeprogress_fcstexp/{}-{}.png'.format(stime_bgn,stime_end)

    fmt_f = os.path.join(dir_ClimaX,
              ('speedy-climax/211_val-climax/gnu_cat/'+\
               'prepbufr_CLIMAXint-0LETKF_M000020L0600_inf1.00_{{}}{{:4.2f}}_ad0.00_po0.00_'+\
               'SMP{}-{}_fcst_glb_{{}}.txt').format(stime_bgn,stime_end))
    f_RTPS_rmse = fmt_f.format('RTPS',1.20,'RMSE')
    f_RTPS_sprd = fmt_f.format('RTPS',1.20,'SPRD')
    f_RTPP_rmse = fmt_f.format('RTPP',0.90,'RMSE')
    f_RTPP_sprd = fmt_f.format('RTPP',0.90,'SPRD')

    iz = get_iz(var, lev)
    unit = get_unit(var)

    RTPS_rmse, RTPS_sprd, RTPP_rmse, RTPP_sprd = [], [], [], []
    for line in open(f_RTPS_rmse,'r').readlines():
        RTPS_rmse.append(float(line.strip().split()[2+iz]))

    for line in open(f_RTPS_sprd,'r').readlines():
        RTPS_sprd.append(float(line.strip().split()[2+iz]))

    for line in open(f_RTPP_rmse,'r').readlines():
        RTPP_rmse.append(float(line.strip().split()[2+iz]))

    for line in open(f_RTPP_sprd,'r').readlines():
        RTPP_sprd.append(float(line.strip().split()[2+iz]))

    figsize = (7.5,4)
    grd_kw = dict(left=0.1, right=0.95, bottom=0.15, top=0.9,
                  wspace=0.1) 
    fs_title = 12
    fs_label = 10
    lw = dict(RMSE=1.2, SPRD=1.2)
    color = dict(RTPS='dodgerblue', RTPP='tomato')

    xticks = np.linspace(0,20,6)
    xticklabels = [int(v) for v in xticks*6]

    fig, ax = plt.subplots(figsize=figsize, gridspec_kw=grd_kw)
    ax.plot(RTPS_rmse, linewidth=lw['RMSE'], color=color['RTPS'], 
            label='RMSE (RTPS:{:4.2f})'.format(alpha['RTPS']))
    ax.plot(RTPS_sprd, linewidth=lw['SPRD'], color=color['RTPS'], linestyle='dashed',
            label='Spread (RTPS:{:4.2f})'.format(alpha['RTPS']))
    ax.plot(RTPP_rmse, linewidth=lw['RMSE'], color=color['RTPP'], 
            label='RMSE (RTPP:{:4.2f})'.format(alpha['RTPP']))
    ax.plot(RTPP_sprd, linewidth=lw['SPRD'], color=color['RTPP'], linestyle='dashed',
            label='Spread (RTPP:{:4.2f})'.format(alpha['RTPP']))
    ax.legend()
    ax.set_xlim(xticks[0], xticks[-1])
    ax.tick_params(direction='in')
    ax.set_xticks(xticks, xticklabels)
    ax.set_xlabel('Forecast lead time (hr)', fontsize=fs_label)
    ax.set_ylabel('RMSE, Spread [{}]'.format(unit), fontsize=fs_label)
    ax.set_title('{} [{}] at {} hPa'.format(var,unit,plv[lev-1]), fontsize=fs_title)
    fig.savefig(figname, bbox_inches='tight', pad_inches=0.1, dpi=150)
    plt.show()


def draw_timeseries_nobs():
    dirname = args[2]
    year = int(args[3])

    nmsg = 20

    lst_var = ['P', 'Q', 'T', 'U', 'V']

    # Check log files
    now = datetime.datetime(year, 1, 1, 0)
    lst_date_not_ok = []
    while now.year == year:
        str_date = now.strftime("%Y%m%d%H")
        #print(str_date)
        f = '{}/log/{}'.format(dirname, str_date)
        if not os.path.isfile(f):
            print('File not found: {}'.format(f))

        is_ok = False
        line = subprocess.run(['tail', '-n', '1', f], encoding='utf-8',
                              stdout=subprocess.PIPE).stdout
        dat = line.strip().split()
        if len(dat) > 1:
            if dat[0:3] == ['[-','program','main']:
                is_ok = True
        if not is_ok:
            print(str_date, line.strip())
            lst_date_not_ok.append(str_date)
        now += datetime.timedelta(hours=6)
    if len(lst_date_not_ok) > 0:
        s = ''
        for str_date in lst_date_not_ok:
            s += ' '+str_date
        print(s)
        return

    # Read log files
    lst_d_vald = []
    lst_d_thin = []

    now = datetime.datetime(year, 1, 1, 0)
    while now.year == year:
        str_date = now.strftime("%Y%m%d%H")
        #print(str_date)
        f = '{}/log/{}'.format(dirname, str_date)
        fp = open(f,'r')

        while True:
            line = fp.readline().strip()
            if line == '': continue
            if line == 'Report of time=0': break

        fp.readline()
        fp.readline()  # TOTAL
        d_vald = {}
        d_thin = {}
        imsg = 0
        switch = 'used'

        lines = fp.readlines()
        fp.close()

        il = -1
        for imsg in range(nmsg):
            il += 1
            dat = lines[il].strip().split()
            msgtyp = dat[0]
            d_thin[msgtyp] = {'SUM': int(dat[2])}
            d_vald[msgtyp] = {'SUM': int(dat[4])}
            if d_thin[msgtyp]['SUM'] > 0:
                il += 1
                while True:
                    il += 1
                    dat = lines[il].strip().split()
                    var = dat[0]
                    if var not in lst_var:
                        il -= 1
                        break
                    d_thin[msgtyp][var] = [int(n) for n in dat[1:7]] # Qmiss-Q4
            if d_vald[msgtyp]['SUM'] > 0:
                il += 1
                while True:
                    il += 1
                    dat = lines[il].strip().split()
                    var = dat[0]
                    if var not in lst_var:
                        il -= 1
                        break
                    d_vald[msgtyp][var] = [int(n) for n in dat[1:7]] # Qmiss-Q4

        lst_d_vald.append(d_vald)
        lst_d_thin.append(d_thin)

        now += datetime.timedelta(hours=6)

    nt = len(lst_d_vald)
    lst_msgtyp = list(lst_d_thin[0].keys())
    print(lst_msgtyp)

    tms_thin = {}
    tms_vald = {}
    for var in lst_var:
        tms_thin[var] = {}
        tms_vald[var] = {}
        for msgtyp in lst_msgtyp:
            tms_thin[var][msgtyp] = []
            tms_vald[var][msgtyp] = []
            for d_thin in lst_d_thin:
                if var in d_thin[msgtyp].keys():
                    tms_thin[var][msgtyp].append(sum(d_thin[msgtyp][var]))
                else:
                    tms_thin[var][msgtyp].append(0)
            for d_vald in lst_d_vald:
                if var in d_vald[msgtyp].keys():
                    tms_vald[var][msgtyp].append(sum(d_vald[msgtyp][var]))
                else:
                    tms_vald[var][msgtyp].append(0)
            tms_thin[var][msgtyp] = np.array(tms_thin[var][msgtyp])
            tms_vald[var][msgtyp] = np.array(tms_vald[var][msgtyp])

    colors = {
      'ADPUPA': 'deepskyblue',
      'AIRCFT': 'orange',
      'SATWND': 'hotpink',
      'PROFLR': 'brown',
      'VADWND': 'gold',
      'ADPSFC': 'forestgreen',
      'SFCSHP': 'royalblue',
      'ASCATW': 'plum',
    }

    lw = 1
    t_all = range(nt)
    #-----------------------------------------------------------
    #
    #-----------------------------------------------------------
    def draw_var(tms, nam, var):
        fig = plt.figure()
        for msgtyp in lst_msgtyp:
            if tms[var][msgtyp].max() > 0:
                if msgtyp == 'ADPUPA':
                    vmean = tms[var][msgtyp][0::2].mean()
                    plt.plot(t_all[0::2], tms[var][msgtyp][0::2], 
                             color=colors[msgtyp], linewidth=lw, 
                             label='{}_0 ({})'.format(msgtyp, int(vmean)))
                    vmean = tms[var][msgtyp][1::2].mean()
                    plt.plot(t_all[1::2], tms[var][msgtyp][1::2], 
                             color=colors[msgtyp], linewidth=lw, linestyle='dashed',
                             label='{}_1 ({})'.format(msgtyp, int(vmean)))
                else:
                    vmean = tms[var][msgtyp].mean()
                    plt.plot(t_all, tms[var][msgtyp], 
                             color=colors[msgtyp], linewidth=lw, 
                             label='{} ({})'.format(msgtyp, int(vmean)))

        plt.legend()
        plt.title('{} {}'.format(var, nam))

        return fig
    #-----------------------------------------------------------
    #
    #-----------------------------------------------------------
    figdir = os.path.join(dirname,figtopdir+'/timeseries_nobs')
    os.makedirs(figdir,exist_ok=True)

    for var in lst_var:
        print(var)
        for msgtyp in lst_msgtyp:
            print('{} {:6.1f} {:6.1f}'.format(
                  msgtyp, tms_thin[var][msgtyp].mean(), tms_vald[var][msgtyp].mean()))
        fig = draw_var(tms_vald, 'vald', var)
        figname = os.path.join(figdir,'{}_{}_{}.png'.format(var,'vald',year))
        print('Saving {}'.format(figname))
        fig.savefig(figname, bbox_inches='tight', pad_inches=0.1)

        fig = draw_var(tms_thin, 'thin', var)
        figname = os.path.join(figdir,'{}_{}_{}.png'.format(var,'thin',year))
        print('Saving {}'.format(figname))
        fig.savefig(figname, bbox_inches='tight', pad_inches=0.1)

    plt.show()


def draw_map_obs_grid():
    dirname = args[2]
    msgtyp = args[3]
    var = args[4]
    lev = int(args[5])
    stime_s = args[6]
    stime_e = args[7]
    show = str_to_bool(args[8])

    if stime_e[6:8] == '00':
        stime_e = stime_e[:6] + '{:02d}'.format(get_dymax(int(stime_e[:4]),int(stime_e[4:6])))\
                   + stime_e[8:]
        if stime_e[8:10] == '00':
            stime_e = stime_e[:8] + '18'

    dt = datetime.timedelta(hours=6)
    time_s = datetime.datetime(
               int(stime_s[:4]),int(stime_s[4:6]),int(stime_s[6:8]),int(stime_s[8:10]))
    time_e = datetime.datetime(
               int(stime_e[:4]),int(stime_e[4:6]),int(stime_e[6:8]),int(stime_e[8:10]))

    figdir = os.path.join(dirname,figtopdir+'/map_obs_grid/{}-{}'.format(stime_s,stime_e))
    os.makedirs(figdir,exist_ok=True)
    print('Output directory: '+figdir)

    if msgtyp == 'ALL':
        if var in ['U','V']:
            lst_msgtyp = ['ADPUPA','AIRCFT','SFCSHP','VADWND',
                          'SATWND','ASCATW']
        elif var == 'T':
            lst_msgtyp = ['ADPUPA','AIRCFT','SFCSHP']
        elif var == 'Q':
            lst_msgtyp = ['ADPUPA','SFCSHP']
        elif var == 'Ps':
            lst_msgtyp = ['ADPSFC','SFCSHP']
    else:
        lst_msgtyp = [msgtyp]

    if var == 'Ps':
        var_ = 'P'
        lev = ''
    else:
        var_ = var

    num_vald = np.zeros((nlat,nlon))
    num_slct = np.zeros((nlat,nlon))

    time = time_s - dt
    itime = 0
    while True:
        time += dt
        if time > time_e: break
        stime = time.strftime("%Y%m%d%H")
        itime += 1

        for msgtyp_ in lst_msgtyp:
            dhr = 0
            f_num_vald = os.path.join(dirname,'monit/{}/num_vald_{}_{}_{:02d}.bin'\
                                      .format(stime,msgtyp_,var_,dhr))
            f_num_slct = os.path.join(dirname,'monit/{}/num_slct_{}_{}_{:02d}.bin'\
                                      .format(stime,msgtyp_,var_,dhr))
            if not os.path.isfile(f_num_vald): continue
            #print(f_num_vald)
            if var == 'Ps':
                num_vald += np.fromfile(f_num_vald,dtype=np.int32).reshape(nlev,nlat,nlon)[0]
                num_slct += np.fromfile(f_num_slct,dtype=np.int32).reshape(nlev,nlat,nlon)[0]
            else:
                num_vald += np.fromfile(f_num_vald,dtype=np.int32).reshape(nlev,nlat,nlon)[lev-1]
                num_slct += np.fromfile(f_num_slct,dtype=np.int32).reshape(nlev,nlat,nlon)[lev-1]

    num_vald /= float(itime)
    num_slct /= float(itime)

    def imshow(ax, dat, cmap, norm):
        d, latmax = add_margin_pole(dat)
        return ax.imshow(d, cmap=cmap, norm=norm, interpolation='nearest',
                         origin='lower', extent=[0,360,-latmax,latmax],
                         transform=ccrs.PlateCarree())

    #vmax = num_vald.max()*0.8
    #vmax = 10
    #bounds_vald = np.linspace(0,vmax,11)
    bounds = [0,0.1, 0.5, 1, 2, 3, 5, 10]
    cmap = mpl.colors.ListedColormap(
                  ['w']+[plt.cm.Blues(l) for l in np.linspace(0,1,len(bounds)+1)[1:-1]])
    cmap.set_over(plt.cm.Blues(1.))
    norm = mpl.colors.BoundaryNorm(bounds,cmap.N)

    fs = (8,6)
    prj = ccrs.PlateCarree(central_longitude=180)

    #"""
    fig1, ax = make_fig_global(fs, prj)
    im = imshow(ax, num_vald, cmap, norm)
    cb = fig1.colorbar(im, aspect=50, pad=0.04, orientation='horizontal',
                       extend='max', extendfrac=0.1)
    cb.set_ticks(bounds)
    #ax.set_title('Mean number of valid obs, {} {}{}'.format(msgtyp, var, lev))
    savefig(fig1, os.path.join(figdir,'num_vald_{}_{}{}.png'.format(msgtyp,var,lev)))
    #"""

    fig2, ax = make_fig_global(fs, prj)
    im = imshow(ax, num_slct, cmap, norm)
    cb = fig2.colorbar(im, aspect=50, pad=0.04, orientation='horizontal',
                       extend='max', extendfrac=0.1)
    cb.set_ticks(bounds)
    #ax.set_title('Mean number of selected obs, {} {}{}'.format(msgtyp, var, lev))
    savefig(fig2, os.path.join(figdir,'num_slct_{}_{}{}.png'.format(msgtyp,var,lev)))

    if show:
        plt.show()


def draw_map_nobs():
    dir_run = args[2]
    var = args[3]
    lev = int(args[4])
    stime_s = args[5]
    stime_e = args[6]
    show = str_to_bool(args[7])

    iz = get_iz(var, lev)

    if dir_run[-1] == '/':
        dir_run = dir_run[:-1]
    runname = os.path.basename(dir_run)

    bounds = [0, 10, 100, 200, 300, 500, 700, 1000]
    cmap = mpl.colors.ListedColormap(
             ['w'] + [plt.cm.Blues(ic) for ic in np.linspace(0,1,len(bounds))[1:-1]])
    cmap.set_over(plt.cm.Blues(1.))
    norm = mpl.colors.BoundaryNorm(bounds, cmap.N)

    time_s = datetime.datetime(year=int(stime_s[:4]), month=int(stime_s[4:6]),
                               day=int(stime_s[6:8]), hour=int(stime_s[8:10]))
    time_e = datetime.datetime(year=int(stime_e[:4]), month=int(stime_e[4:6]),
                               day=int(stime_e[6:8]), hour=int(stime_e[8:10]))


    nobs = np.zeros((nlat,nlon))

    dt = datetime.timedelta(hours=6)
    time = time_s - dt
    nt = 0
    while time < time_e:
        time += dt
        nt += 1
        stime = time.strftime("%Y%m%d%H")
        fname = os.path.join(dir_run,'nobs/{}.grd'.format(stime))
        if not os.path.isfile(fname):
            raise Exception('File not found: '+fname)
        nobs += np.fromfile(fname,dtype=np.float32).reshape(-1,nlat,nlon)[iz].byteswap()
    nobs /= nt

    fs = (6,4)

    nobs_plt, latmax = add_margin_pole(nobs)

    fig, ax = make_fig_global(prj=ccrs.PlateCarree(180), fs=fs)
    im = ax.imshow(nobs, cmap=cmap, norm=norm, origin='lower',
                   extent=[0,360,-90,90], transform=ccrs.PlateCarree())
    fig.colorbar(im, aspect=50, pad=0.04, orientation='horizontal',
                 extend='max', extendfrac=0.1)
    savefig(fig, figtopdir+'/nobs/{}/{}-{}/{}{}.png'.format(runname,stime_s,stime_e,var,lev))

    if show:
        plt.show()
    


def draw_map_obs():
    dirname = args[2]
    msgtyp = args[3]
    var = args[4]
    lev = int(args[5])
    stime = args[6]

    fname = os.path.join(dirname, 'monit/{}/rec_all_{}_{}_00.txt'.format(stime, msgtyp, var))
    print('Reading {}'.format(fname))

    # Output of SUBROUTINE write_monit_summary > All records
    rec_all = read_rec_all(fname)
    """rec_all = []
    for line in open(fname,'r').readlines()[1:]:
        dat = line.strip().split()
        rec_all.append(dict(
          lon = float(dat[0]),
          lat = float(dat[1]), 
          lev = float(dat[2]),
          iqmk = int(dat[3]),
          obs = float(dat[4]),
          oer = float(dat[5]),
          ilon_near = int(dat[6]), 
          ilat_near = int(dat[7]), 
          ilev_near = int(dat[8]), 
          ilon_blng = int(dat[9]), 
          ilat_blng = int(dat[10]), 
          ilev_blng = int(dat[11]), 
          wgtmax = float(dat[12]),
          is_valid = str_to_bool(dat[13]),
          is_selected = str_to_bool(dat[14])
        ))"""

    sz = 50
    lw = 1
    cmap = plt.cm.jet

    # 
    plt_selected = dict(lon=[], lat=[], obs=[], iqmk=[])
    plt_valid   = copy.deepcopy(plt_selected)
    plt_invalid = copy.deepcopy(plt_selected)
    for rec in rec_all:
        if rec['ilev_near'] != lev: continue
        if rec['obs'] == PREPBUFR_miss: continue
        if rec['is_selected']:
            plt_selected['lon'].append(rec['lon'])
            plt_selected['lat'].append(rec['lat'])
            plt_selected['obs'].append(rec['obs'])
            plt_selected['iqmk'].append(rec['iqmk'])
        elif rec['is_valid']:
            plt_valid['lon'].append(rec['lon'])
            plt_valid['lat'].append(rec['lat'])
            plt_valid['obs'].append(rec['obs'])
            plt_valid['iqmk'].append(rec['iqmk'])
        else:
            plt_invalid['lon'].append(rec['lon'])
            plt_invalid['lat'].append(rec['lat'])
            plt_invalid['obs'].append(rec['obs'])
            plt_invalid['iqmk'].append(rec['iqmk'])
    for key in plt_selected.keys():
        plt_selected[key] = np.array(plt_selected[key])
        plt_valid[key]    = np.array(plt_valid[key])
        plt_invalid[key]  = np.array(plt_invalid[key])

    if len(plt_selected['obs']) == 0 and len(plt_valid['obs']) == 0:
        print('No data')
        return
    elif len(plt_selected['obs']) == 0:
        vmin = plt_valid['obs'].min()
        vmax = plt_valid['obs'].max()
    elif len(plt_valid['obs']) == 0:
        vmin = plt_selected['obs'].min()
        vmax = plt_selected['obs'].max()
    else:
        vmin = min(plt_selected['obs'].min(), plt_valid['obs'].min())
        vmax = max(plt_selected['obs'].max(), plt_valid['obs'].max())
    print(vmin, vmax)

    def draw_each(rec_plt, nam):
        print(nam, len(rec_plt['lon']))
        fig, ax = make_fig_global()
        maskmiss = rec_plt['iqmk'] == -1
        mask1 = (rec_plt['iqmk'] == 0) | (rec_plt['iqmk'] == 1)
        mask2 = rec_plt['iqmk'] == 2
        mask3 = rec_plt['iqmk'] == 3
        mask4 = rec_plt['iqmk'] >= 4
        for mask, marker in zip(
          [mask4, mask3, mask2, mask1, maskmiss], ['v', 's', 'o', '*', '^']):
            ax.scatter(rec_plt['lon'][mask], rec_plt['lat'][mask],
                       s=sz, c=rec_plt['obs'][mask], cmap=cmap, edgecolor='k', linewidth=1,
                       vmin=vmin, vmax=vmax, marker=marker)
        ax.set_title(nam)
        return fig, ax

    west, east, south, north = -180, 180, -90, 90
    #west, east, south, north = -34, 9, 43, 70
    west, east, south, north = -160, -115, 34, 61
    #-----------------------------------------------------------
    #
    #-----------------------------------------------------------
    #"""
    fig = plt.figure()
    ax = fig.add_subplot(projection=ccrs.PlateCarree())
    ax.gridlines(draw_labels=['left','bottom'], 
                 xlocs=np.arange(-180,181,5), ylocs=np.arange(-90,91,5),
                 color='none')
    ax.set_extent((west,east,south,north))
    ax.coastlines(linewidth=0.5, zorder=0)
    ax.scatter(plt_invalid['lon'], plt_invalid['lat'], s=sz,
               marker='x', facecolor='k')
    ax.scatter(plt_valid['lon'], plt_valid['lat'], s=sz,
               marker='o', facecolor='gray', edgecolor='gray', zorder=1)
    ax.scatter(plt_selected['lon'], plt_selected['lat'], s=sz,
               marker='o', facecolor='w', edgecolor='k', zorder=2)
    ax.set_title('{} {}{}'.format(msgtyp, var, lev))
    savefig(fig, os.path.join(dirname,figtopdir+'/map_obs_thinning/{}_{}_{}_{}.png'\
                              .format(msgtyp,var,lev,stime)))
    #"""

    plt.show()



def draw_map_obs_overview():
    msgtyp = args[2]
    var = args[3]
    lev = int(args[4])
    stime_s = args[5]
    stime_e = args[6]
    is_point = str_to_bool(args[7])

    nlon, nlat = 3600, 1800

    time_s = datetime.datetime(
               int(stime_s[:4]),int(stime_s[4:6]),int(stime_s[6:8]),int(stime_s[8:10]))
    time_e = datetime.datetime(
               int(stime_e[:4]),int(stime_e[4:6]),int(stime_e[6:8]),int(stime_e[8:10]))
    dt = datetime.timedelta(hours=6)

    dir_obs_top = 'out/fig'
    #dir_obs_top = 'tmp'

    if var == 'Ps':
        ilev = 1
    else:
        ilev = lev

    if is_point:
        n = 0
        time = time_s - dt
        while time < time_e:
            time += dt
            stime = time.strftime('%Y%m%d%H')
            #print(stime)

            dir_obs = os.path.join(dir_obs_top,'{}'.format(stime))
            if not os.path.isdir(dir_obs): continue
            #f_obs = os.path.join(dir_obs,'{}_{}{}.bin'.format(msgtyp,var,ilev))
            f_obs = os.path.join(dir_obs,'{}_{}{}_p.bin'.format(msgtyp,var,ilev))
        
            if not os.path.isfile(f_obs): continue
            n += int(os.path.getsize(f_obs)/(4*4))
        if n == 0:
            return

        lon = np.empty((n))
        lat = np.empty((n))
        obs = np.empty((n))
        wgt = np.empty((n))

        n = 0
        time = time_s - dt
        while time < time_e:
            time += dt
            stime = time.strftime('%Y%m%d%H')
            print(stime)

            dir_obs = os.path.join(dir_obs_top,'{}'.format(stime))
            if not os.path.isdir(dir_obs): continue
            #f_obs = os.path.join(dir_obs,'{}_{}{}.bin'.format(msgtyp,var,ilev))
            f_obs = os.path.join(dir_obs,'{}_{}{}_p.bin'.format(msgtyp,var,ilev))
            if not os.path.isfile(f_obs): continue
  
            #dat = np.fromfile(f_obs,dtype=np.float32).reshape(3,-1)
            dat = np.fromfile(f_obs,dtype=np.float32).reshape(4,-1)
            dsize = dat.shape[1]
            lon[n:n+dsize] = dat[0]
            lat[n:n+dsize] = dat[1]
            obs[n:n+dsize] = dat[2]
            wgt[n:n+dsize] = dat[3]
            n += dsize
        print('n: {}'.format(n))
    else:
        lon = np.zeros((nlat,nlon))
        lat = np.zeros((nlat,nlon))
        obs = np.zeros((nlat,nlon))
        wgt = np.zeros((nlat,nlon))

        time = time_s - dt
        while True:
            time += dt
            if time > time_e: break
            stime = time.strftime('%Y%m%d%H')
            
            dir_obs = os.path.join(dir_obs_top,'{}'.format(stime))
            if not os.path.isdir(dir_obs): continue
            f_obs = os.path.join(dir_obs,'{}_{}{}_g.bin'.format(msgtyp,var,ilev))

            if not os.path.isfile(f_obs): continue
            print(f_obs)

            #"""
            dat = np.fromfile(f_obs,dtype=np.float32).reshape(6,-1)
            ilon = dat[0].astype(np.int32)-1
            ilat = dat[1].astype(np.int32)-1
            lon[ilat,ilon] += dat[2]*dat[5]
            lat[ilat,ilon] += dat[3]*dat[5]
            obs[ilat,ilon] += dat[4]*dat[5]
            wgt[ilat,ilon] += dat[5]
            #"""

            """
            dat = np.fromfile(f_obs,dtype=np.float32).reshape(4,nlat,nlon)
            lon += dat[0]*dat[3]
            lat += dat[1]*dat[3]
            obs += dat[2]*dat[3]
            wgt += dat[3]
            """
        lon = lon[wgt>0] / wgt[wgt>0]
        lat = lat[wgt>0] / wgt[wgt>0]
        obs = obs[wgt>0] / wgt[wgt>0]
        wgt = wgt[wgt>0]

        n = obs.size
        print('n: {}'.format(n))

    arg = np.argsort(wgt)
    lon = lon[arg]
    lat = lat[arg]
    obs = obs[arg]
    wgt = wgt[arg]

    if var == 'Q':
        obs *= 1e-6

    if var == 'Ps':
        slev = ''
    else:
        slev = str(lev)

    def get_vrange(obs,var):
        print('obs min: {:9.2e}, max: {:9.2e}'.format(obs.min(),obs.max()))
        n = obs.size
        #nedge = int(n*0.01*0.5)
        nedge = int(n*0.1)
        print('nedge: {}'.format(nedge))
        obs = np.sort(obs)
        vmin = obs[nedge:n-nedge+1].min()
        vmax = obs[nedge:n-nedge+1].max()
        print('obs min: {:9.2e}, max: {:9.2e}'.format(vmin,vmax))
        if var in ['U','V']:
            vmax = max(abs(vmin),abs(vmax))*vrange_shrink
            vmin = -vmax
        elif var in ['T']:
            vmid = (vmin+vmax)*0.5
            vmax = vmid + (vmax-vmid)*vrange_shrink
            vmin = vmid + (vmin-vmid)*vrange_shrink
        elif var in ['Q']:
            vmin = 0
            vmax = vmax*vrange_shrink
        elif var in ['Ps']:
            vmid = (vmin+vmax)*0.5
            vmax = vmid + (vmax-vmid)*vrange_shrink
            vmin = vmid + (vmin-vmid)*vrange_shrink
        else:
            raise Exception('Invalid value in $var: {}'.format(var))
        return vmin, vmax

    cmap = {
      'U': plt.cm.PRGn, 
      'V': plt.cm.PRGn,
      'T': plt.cm.jet,
      'Q': plt.cm.jet,
      'Ps': plt.cm.viridis,
    }

    vrange_shrink = 0.95
    vmin, vmax = get_vrange(obs,var)
    print('vmin: {:9.2e}, vmax: {:9.2e}'.format(vmin,vmax))

    fs = (8,6)
    s = 2

    fig, ax = make_fig_global(fs, ccrs.Robinson(central_longitude=180))
    im = ax.scatter(lon, lat, c=obs, cmap=cmap[var], s=s, 
                    vmin=vmin, vmax=vmax,
                    transform=ccrs.PlateCarree())
    fig.colorbar(im, orientation='horizontal', aspect=50, pad=0.05, shrink=0.8,
                 extend='both', extendfrac=0.1)
    ax.set_title('{} {}{}'.format(msgtyp, var, slev))

    figdir = figtopdir+'/obs_overview'
    os.makedirs(figdir,exist_ok=True)
    figname = os.path.join(figdir,'{}_{}{}_{}-{}.png'.format(msgtyp, var, slev, stime_s, stime_e))
    print('Saving '+figname)
    fig.savefig(figname, bbox_inches='tight', pad_inches=0.1, dpi=300)

    plt.show()



def draw_map_sfc_modif():
    fname = args[2]

    info, lon, lat = [], [], []
    z0, T0, Ps0 = [], [], []
    z, T, Ps = [], [], []
    for line in open(fname,'r').readlines()[1:]:
        dat = line.strip().split()
        if dat[0] == '#': break
        info.append(int(dat[0]))
        lon.append(float(dat[3]))
        lat.append(float(dat[4]))
        z0.append(float(dat[5]))
        T0.append(float(dat[6]))
        Ps0.append(float(dat[7]))
        z.append(float(dat[8]))
        T.append(float(dat[9]))
        Ps.append(float(dat[10]))
    info = np.array(info)
    lon = np.array(lon)
    lat = np.array(lat)
    z0 = np.array(z0)
    T0 = np.array(T0)
    Ps0 = np.array(Ps0)
    z = np.array(z)
    T = np.array(T)
    Ps = np.array(Ps)

    def draw_var(v0, v1, varname):
        mask = info == 0
        vmin = min(v0.min(), v1.min())
        vmax = max(v0.max(), v1.max())
        figs = []
        for v, name in zip([v0,v1],[varname+'0',varname]):
            fig = plt.figure()
            ax = fig.add_subplot(projection=ccrs.PlateCarree())
            ax.set_global()
            ax.coastlines(linewidth=0.5)
            im = ax.scatter(lon[mask], lat[mask], c=v[mask],
                            s=10, cmap=plt.cm.jet, vmin=vmin, vmax=vmax)

            # TODO gridlines

            fig.colorbar(im, aspect=50, pad=0.08, orientation='horizontal')
            ax.set_title(name)
            figs.append(fig)

        return figs[0], figs[1]

    fig_z0, fig_z = draw_var(z0, z, 'z')

    plt.show()

    plt.close(fig_z0)
    plt.close(fig_z)


if __name__ == '__main__':
    args = sys.argv
    job = args[1]

    if job == 'fields_var':
        draw_fields_var()

    elif job == 'fields':
        draw_fields()

    elif job == 'timeseries_stats':
        draw_timeseries_stats()

    elif job == 'vplot_diff':
        draw_vplot_diff()

    elif job == 'timeprogress_fcstexp':
        draw_timeprogress_fcstexp()

    elif job == 'timeseries_nobs':
        draw_timeseries_nobs()

    elif job == 'map_obs_grid':
        draw_map_obs_grid()

    elif job == 'map_nobs':
        draw_map_nobs()

    elif job == 'map_obs':
        draw_map_obs()

    elif job == 'map_obs_overview':
        draw_map_obs_overview()

    elif job == 'map_sfc_modif':
        draw_map_sfc_modif()

    else:
        raise Exception('Invalid value in $job: {}'.format(job))

# -*- coding: utf-8 -*-
"""
Created on Mon Mar 30 18:32:55 2015

@author: Cheryl
"""

import numpy as np
import pandas as pd
import Tkinter
import tkFileDialog
import datetime
import matplotlib.pyplot as plt

import seaborn as sns

def readfile(msg):
    """open dialog to pick file, reads csv file into data
    returns false if file cannot be read"""
    check = raw_input("Import " + msg + "? Y/N?")
    if check == 'N':
        exit()
    root = Tkinter.Tk()  
    root.withdraw()
    filename = tkFileDialog.askopenfilename(parent = root, 
                                            filetypes=[("Text files","*.csv")])
    try:
        f = pd.read_csv(filename)
        return(f)
    except:
        return(False)
        
#--------------------------------------------------
def sampleintervals(df,lb):
    """ function plots sampling intervals for the 10 best/worst water quailty
    args: df = data frame of river data
    lbestworse = list contains dataframes from best/worse function
    """
    #get intervals between samples for each site, set first to zero for each
    df=df.sort(['site','date'])
    df['lastsample'] = df['date'].shift(1)
    df['lastsite'] = df['site'].shift(1)
    df = df.fillna(value=0)
    
    df = df.fillna(value=0)
    
    df['interval'] = np.where((df['lastsite'] == df['site']), 
             (df['date'] - df['lastsample']).astype('timedelta64[D]')
             , 0) 
    
    #calc mean intervals for data
    dfinterval = df[['site','interval']].groupby(['site'],
        as_index =False).mean().sort('interval')
    print("mean intervals by site")
    print(dfinterval)
    
    dfbest = lb[1].head(10)
    dfworst = lb[1].tail(10)
    # get top 10 , bottom 10
    top = dfbest['site'].tolist()
    bot = dfworst['site'].tolist()
    
    # days to values
    #sdate = min(dfriver['date'])
    
    #look at frequency of top 10
    sns.set(style="white")
    dftop = df[df['site'].isin(top)]
    print("Sample frequency for top 10")
    sns.violinplot(vals = dftop['interval'], groupby = dftop['site'],
                   inner = 'points', color = 'Set3', vert = False,
                   title = "Sample frequency for all years for 10 best")
    plt.draw()
                   
    # look at frequency of bottom 10   
    dfbot = df[df['site'].isin(bot)]
    print(sns.violinplot(vals = dfbot['interval'], groupby = dfbot['site'],
                   inner = 'points', color = 'Set3', vert = False))
    plt.draw()
#--------------------------------------------------
               

def bestworst(df):
    """Create list and graphs of best /worst rivers
    args:
        df:  pandas data frame with river data"""
    # pull post recent 2 years
    dftwoyears = df[df['date'] >= datetime.datetime(2012,1,1)]
    #calc mean
    dfbysite = dftwoyears[['site','samplecount','enterocount']]
    dfbysite = dfbysite.groupby(['site','samplecount'],as_index = False).mean().sort(('enterocount'), ascending=False)
    dfbysite = dfbysite.sort(('enterocount'), ascending=False)
    
    dfworst = dfbysite.head(15).sort(('enterocount'), ascending=False)
    dfbest = dfbysite.tail(15).sort(('enterocount'), ascending=False)
    #
    plt.xticks(rotation=30) 
    sns.set(style="white")
    x = sns.dark_palette("green",15)
    sns.set_palette(x,n_colors = 15)
    dfworst.plot('site','enterocount', 
                     kind='barh', color=x, title = "15 worst, 2 year avg")
    print('Worst 15 for 2 year Average')
    print(dfworst)
     
    
    x = sns.light_palette("green",15, reverse = True)
    sns.set_palette(x,n_colors = 15)
    dfbest.plot('site','enterocount', 
                     kind='barh',color = x,title = "15 best, 2 year avg")
    print('Best 15 for 2 year Average')
    print(dfbest)
    lbestworst = [dfbest,dfworst]
    return(lbestworst)
#-------------------------------------------------
def relate(df, lb):
    # All Together
    sns.cubehelix_palette(4, 1.5, .75, light=.6, dark=.2)
    sns.lmplot("fourdayrain", "enterocount", df, hue="site", 
               palette="Set3", fit_reg=False,legend = False)
       
    sns.residplot(df['fourdayrain'], df['enterocount'])
    
    # not very informative
    
    
    sns.lmplot("fourdayrain", "enterocount", hue="site", data=df,
                     size=4,fit_reg = False, legend = False,
                     palette="PuBuGn")
                     
    #by factors: 
    
    sns.lmplot("fourdayrain", "enterocount", col = 'site', hue="site", data=df,
               col_wrap=5, ci=None, palette="PuBuGn", size=4,
               scatter_kws={"s": 50, "alpha": 1})
    # relationship exists - for some sites have a much more dramatic relationship
    
    # look at worst/best
    dfbest = lb[1].head(10)
    dfworst = lb[1].tail(10)
    # get top 10 , bottom 10
    top = dfbest['site'].tolist()
    bot = dfworst['site'].tolist()
    
    #look at best - not much relationship
    sns.set(style="white")
    dftop = df[df['site'].isin(top)]
    sns.lmplot("fourdayrain", "enterocount", dftop, hue="site", 
               palette="PuBuGn", fit_reg=False)
       
    sns.residplot(dftop['fourdayrain'], dftop['enterocount'])
    
    sns.lmplot("fourdayrain", "enterocount", col = 'site', hue="site", data=dftop,
               col_wrap=2, ci=None, palette="PuBuGn", size=4,
               scatter_kws={"s": 50, "alpha": 1})
               
    #look at worst - a little more obvious 
    dfbot = df[df['site'].isin(bot)]
    sns.lmplot("fourdayrain", "enterocount", dfbot, hue="site", 
               palette="PuBuGn", fit_reg=False)
       
    sns.residplot(dfbot['fourdayrain'], dfbot['enterocount'])
    
    sns.lmplot("fourdayrain", "enterocount", col = 'site', hue="site", data=dfbot,
               col_wrap=2, ci=None, palette="PuBuGn", size=4,
               scatter_kws={"s": 50, "alpha": 1})
               
               
#--------------------------------------------------
# Main Body
#--------------------------------------------------
#get river data
msg = " Nasty Water Data "
dfriver = readfile(msg)
if dfriver.empty:
    print "Error reading file"
    exit()
    
# notes about this data
# Sample Date: Date sample was collected
# Enterocount: The # found per 100ML without dillution, from <10 up to 2419
# Fourdayraintotal:  The combined rainfall for the day of sampling, prior day, 
    #two days prior and three days prior.
    #More than 1/4 inch is considered a “wet weather sample.”  
# SampleCount: The total number of samples for a given site
    # used to calculate mean,max,min
    
#give data col names: 
dfriver.index.name = 'row'
colnames = ['site', 'date', 'enterocount', 'fourdayrain', 'samplecount']
dfriver.columns = colnames

# clean data
dfriver['date'] = pd.to_datetime(dfriver['date'])
dfriver['enterocount'] = dfriver['enterocount'].str.replace('<|>', '')
dfriver = dfriver.convert_objects(convert_numeric=True)
dfriver = dfriver.sort('date')

#  Question 1: Graph best/worst for 2 year avg
lb = bestworst(dfriver)

#  Question 2: Testing interval
# after giving many many attempts at plotting date/time 
sampleintervals(dfriver,lb)

# Question 3:  Rain vs. Water Quailty
relate(dfriver,lb)

plt.show
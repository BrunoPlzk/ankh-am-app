# PACKAGES
#Operating system:
import os
# Stock market
from pandas_datareader import data
import yfinance as yf
#maths
import numpy as np
#Data Manipulation
import pandas as pd
#Dates
import datetime
#Emails:
import smtplib
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from email.mime.base import MIMEBase
from email import encoders


#Path Linux remote instance
if "brunopilarczyk" in os.getcwd():
    path = "/Users/brunopilarczyk/Documents/Docs/ANKH-AM/ankh-am-app/"
else:
    path = "/home/ec2-user/ankh-am/bbands-signal/"

#Useful functions:
def recommendation(low, maximum, up, down):
    if maximum >= up:
        reco = "SELL"
    elif low <= down:
        reco = "BUY"
    else:
        reco = "HOLD"
    return reco 

#Importing companies along with their tickers
bbands = pd.read_csv(path+"bbands.csv")
symbols = pd.read_csv(path+"tickers.csv")

df_signal = pd.DataFrame(columns = ["Company",
                                    "Ticker",
                                    "HIGH",
                                    "LOW",
                                    "MA",
                                    "UP",
                                    "DOWN",
                                    "Recommendation"])

#Setting up the date:
days_ago = datetime.datetime.now() + datetime.timedelta(days = -60)
days_ago = datetime.datetime.strftime(days_ago, format = "%Y-%m-%d")

#Looping over all the companies:
for ticker in symbols["Ticker"]:
    
    #Collecting company name
    company = symbols[symbols["Ticker"] == ticker]["Company"].iloc[0]
    
    print("Predicting recommendation for "+company+"...")
    
    #Collecting optimal Moving Average window and Standard Deviation multiplier:
    n_MA = np.round(bbands[bbands["company"] == company]["MA_opt"].iloc[0],4)
    x_SD = np.round(bbands[bbands["company"] == company]["SD_opt"].iloc[0],4)
    
    #Try/Except method in case of failing to gather data from yahoo finance:
    try:
        # Importing data:
        pre_df = yf.Ticker(ticker)
        df = pre_df.history(period = "2mo")
        df = df.dropna(axis = 0)

        # With pandas data reader:  
        #df = data.DataReader(ticker, "yahoo")
        df["Real"] = (df["High"] + df["Low"] + df["Close"])/3
        
        #Calculation MA, SD --> UP and DOWN:
        df["MA"] = df['Real'].rolling(n_MA).mean()
        df["SD"] = df['Real'].rolling(n_MA).std(0)
        df["UP"] = df["MA"] + x_SD * df["SD"]
        df["DOWN"] = df["MA"] - x_SD * df["SD"]
        
        #Keeping only today's price: 
        df = df.iloc[len(df)-1]
    
        #Recommendation system:
        reco = recommendation(df["Low"], df["High"], df["UP"], df["DOWN"])
        
        
    except:
        reco = "FAIL"
    
    #Define row to promote to signal dataframe:
    df_promote = pd.DataFrame({"Company" : [company],
                               "Ticker" : [ticker],
                               "HIGH" : [df["High"]],
                               "LOW" : [df["Low"]],
                               "MA" : [df["MA"]],
                               "UP" : [df["UP"]],
                               "DOWN" : [df["DOWN"]],
                               "Recommendation" : [reco]
                               }) 
     
    #Appending to df_signal
    df_signal = df_signal.append(df_promote)
    
    #### END OF THE LOOP ####


#For tickers which failed, 2nd loop:
failed_tickers = df_signal[df_signal["Recommendation"] == "FAIL"]["Ticker"].to_list()

#If the list of failed tickers is not empty, loop again:
if len(failed_tickers) != 0:
    for ticker in failed_tickers:
        #Collecting company name
        company = symbols[symbols["Ticker"] == ticker]["Company"].iloc[0]
            
            
        #Collecting optimal Moving Average window and Standard Deviation multiplier:
        n_MA = np.round(bbands[bbands["company"] == company]["MA_opt"].iloc[0],4)
        x_SD = np.round(bbands[bbands["company"] == company]["SD_opt"].iloc[0],4)
            
        #Try/Except method in case of failing to gather data from yahoo finance:
        try:
            #Importing data:
            df = data.DataReader(ticker, "yahoo", start = days_ago)
            df["Real"] = (df["High"] + df["Low"] + df["Close"])/3
            
            #Calculation MA, SD --> UP and DOWN:
            df["MA"] = df['Real'].rolling(n_MA).mean()
            df["SD"] = df['Real'].rolling(n_MA).std(0)
            df["UP"] = df["MA"] + x_SD * df["SD"]
            df["DOWN"] = df["MA"] - x_SD * df["SD"]
            
            #Keeping only today's price: 
            df = df.iloc[len(df)-1]
        
            #Recommendation system:
            reco = recommendation(df["Low"], df["High"], df["UP"], df["DOWN"])
            
            
        except:
            reco = "FAIL"
            
        #Define row to promote to signal dataframe:
        df_promote = pd.DataFrame({"Company" : [company],
                                   "Ticker" : [ticker],
                                   "Recommendation" : [reco]
                                   }) 
        
        print("Predicting recommendation for "+company+":", reco)
        
        #Appending to df_signal
        df_signal.loc[df_signal["Ticker"] == ticker] = df_promote

        #### END OF THE LOOP ####
        

#Reset index:
df_signal.reset_index(drop = True, inplace = True)

#Collecting today's date:
today = datetime.datetime.now()
today = datetime.datetime.strftime(today, format = "%Y-%m-%d")


#Saving signal dataframe to csv file in the working directory:
filename = "bbands"+"-"+"recommandation"+today+".csv"
df_signal.to_csv(path+filename, index = False)

#Collecting and converting into lists BUY, SELL and FAIL series:
BUY_list = df_signal[df_signal["Recommendation"] == "BUY"]["Company"].to_list()
SELL_list = df_signal[df_signal["Recommendation"] == "SELL"]["Company"].to_list()
FAIL_list = df_signal[df_signal["Recommendation"] == "FAIL"]["Company"].to_list()


#Adding right HTML elements to the words inside the lists:
for listing in [BUY_list, SELL_list, FAIL_list]:
    for i in range(len(listing)):
        listing[i] = "<li>"+listing[i]+"</li>"


#Binding words with spaces into a string:
BUY_list_mail = "".join(map(str, BUY_list))
SELL_list_mail = "".join(map(str, SELL_list))
FAIL_list_mail = "".join(map(str, FAIL_list))

#Checking conditions where there is no company to recommend in one list:
if BUY_list_mail == "":
    BUY_list_mail = "There is no stock to buy today."
    
if SELL_list_mail == "":
    SELL_list_mail = "There is no stock to sell today."
    
if FAIL_list_mail == "":
    FAIL_list_mail = "No recommendations failed during the process."
    

#Sending the mail:
    
#Credentials
MY_ADDRESS = 'ankh.asset.management@gmail.com'
PASSWORD = 'ru.bru2021'

#Recipients:
recipient1 = ["Pilarczyk", "bruno.plzk@gmail.com"]

#Path Linux remote instance
if "brunopilarczyk" in os.getcwd():
    recipient2 = ["Rousseaux", "magiicsquat@gmail.com"]    
else:
    recipient2 = ["Rousseaux", "rudolph.rousseaux@gmail.com"]

#Attachment:
attachment = open(path+filename, "rb")
part = MIMEBase("application", "octet-stream")
part.set_payload((attachment).read())
encoders.encode_base64(part)
part.add_header("Content-Disposition", "attachment; filename= "+filename)


#Looping over the recipients along with their mail:   
for recipient, mail in recipient1,recipient2:
    print("Sending mail to:", recipient, "|", mail)
    
    logo = """<a href="https://imgbb.com/"><img src="https://i.ibb.co/tPhSRRd/ankh-am-logo.png" alt="ankh-am-logo" border="0"></a>"""
    
    #Body of the mail
    body = "".join(["Dear Mr ",recipient,","
                    "<br>","<br>",
                    "Please find below today's recommendations about stock market decisions to be made.",
                    "<br>", "<br>",
                    "Long positions (<strong>BUY</strong>):","<br>",BUY_list_mail,"<br>",
                    "Short positions (<strong>SELL</strong>):","<br>",SELL_list_mail,"<br>",
                    "<strong>FAIL</strong> list:","<br>",FAIL_list_mail,"<br>","<br>",
                    "Kind regards,","<br>","<br>",
                    logo,
                    "<br>","<br>",
                    "<strong>Contact</strong>: ankh.asset.management.com",
                    "<br>","<br>",
                    ])
    
    #Initiate email
    email = MIMEMultipart()
    
    #Mail recipient 
    email['To'] = mail
    
    #Mail header
    email['From'] = "ANKH Asset Management"
    
    #Subject of the mail
    email['Subject'] = "Recommendation on "+today
    
    #Defining the body as mail content and subtype to HTML
    email.attach(MIMEText(body,"html"))
    #Attaching csv file
    email.attach(part)

    
    #Other contacts
    #email['Cc'] = "ankh.am@outlook.com"
    
                
    smtp_connection = smtplib.SMTP(host='smtp.gmail.com', port = 587)  
    smtp_connection.starttls()
    smtp_connection.login(MY_ADDRESS, PASSWORD)
    status = smtp_connection.send_message(email)
    smtp_connection.quit()



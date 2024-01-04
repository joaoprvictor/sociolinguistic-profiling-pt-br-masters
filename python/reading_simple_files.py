# -*- coding: utf-8 -*-
"""reading_simple_files.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1ErlxG127R6s8m48nvNvfXbb1co9zDoqU
"""

# importing libraries
import glob
import re
import pandas as pd
import tqdm
from datetime import datetime

# setting up the paths
simpleTranscriptPath = "C:\\Users\\joaop\\Desktop\\Corpora\\C-Oral-Brasil\\TextualCorpus\\"
file_test = "C:\\Users\joaop\\Desktop\\Corpora\\C-Oral-Brasil\\dissertation_codes\\file_test\\"
textOutputPath = "C:\\Users\\joaop\\Desktop\\Corpora\\C-Oral-Brasil\\dissertation_codes\\text_output_files\\"

# getting a list of the paths of each file
simpleTranscriptList = glob.glob(f'{file_test}/*.txt')
simpleTranscriptList[4:5]

# getting a list of the paths of each file
simpleTranscriptList = glob.glob(f'{simpleTranscriptPath}/*.txt')
simpleTranscriptList[78:79]

# this function reads, preprocess and convert (into a dataframe) the texts of the simple transcriptions

def read_preprocessFile(TranscriptionList): #str
    dfList = []
    newTurnList = []
    for file in TranscriptionList:
        fileOpen = open(file, "r", encoding="utf-8") # , encoding="utf-8-sig"
        fileName = file.split("\\")[7][:8]
        text = fileOpen.readlines()
        # listTurnList.append(text)
        # fileNameList.append(fileName)
        for turn in text:
            turnClean = re.sub(" +", " ", turn)
            turnClean = re.sub("\+", "_utt", turnClean)
            turnClean = re.sub("\/\/", "_utt", turnClean)
            turnClean = re.sub("\n", "_utt", turnClean)
            # turnClean = re.sub(" $", "_utt", turnClean)
            turnClean = re.sub("_utt _utt", "_utt", turnClean)
            turnClean = re.sub("_utt_utt", "_utt", turnClean)
            # turnClean = re.sub(" $", "", turnClean)
            turnClean = re.sub("\*[A-Z]{3}:", "", turnClean)
            turnClean = re.sub("\[[0-9]+\]", "", turnClean)
            turnClean = re.sub(r"o\\'", "o'", turnClean)
            abb = turn.split(":")[0][1:]
            # abbreviationList.append(fileName+abb)
            newTurnList.append([abb, fileName, turnClean])
    textDf = pd.DataFrame(newTurnList, columns=["acronym", "file", "turn"])

    dfList.append(textDf)
    allSimpleUttDf = pd.concat(dfList)
    return allSimpleUttDf

allSimpleUttDf = read_preprocessFile(simpleTranscriptList)

allSimpleUttDf["utterance"] = allSimpleUttDf.turn.str.split("_utt") #splitting the turns as a list of utterances
allSimpleUttDf = allSimpleUttDf.explode("utterance") # one row = one utterance
allSimpleUttDf = allSimpleUttDf[allSimpleUttDf["utterance"]!=""] # deleting unwanted/unnecessary rows
allSimpleUttDf = allSimpleUttDf[allSimpleUttDf["utterance"]!=" "] # deleting unwanted/unnecessary rows
allSimpleUttDf = allSimpleUttDf[allSimpleUttDf["utterance"]!="_utt"] # deleting unwanted/unnecessary rows
allSimpleUttDf["person_code_in_file"] = allSimpleUttDf.file+allSimpleUttDf.acronym # creating a code per person by file
allSimpleUttDf = allSimpleUttDf[["file", "acronym", "person_code_in_file", "turn", "utterance"]] #reordering the columns
allSimpleUttDf["utterance"] = allSimpleUttDf["utterance"].str.replace(" \/ ", " ") # deleting the non-terminal break markup

len(allSimpleUttDf)

# double checking
# 34167 = the value registered on the CORAL book
subtraction = 34167- len(allSimpleUttDf)

if subtraction > 0:
    print(f"The size of the current dataframe is {len(allSimpleUttDf)}\nThe number of utterances in C-oral is 34167\nThere are {subtraction} utterances missing")
elif subtraction < 0:
    print(f"The size of the current dataframe is {len(allSimpleUttDf)}\nThe number of utterances in C-oral is 34167\nThere are {subtraction*(-1)} extra utterances")
else:
    print("The quantity of utterances are the same! You splitted the utterances correctly")

# creating a column with the overlapped parts of an utterance
allSimpleUttDf["overlapping"] = allSimpleUttDf.utterance.str.replace("\<", "overlap_")
allSimpleUttDf["overlapping"] = allSimpleUttDf.overlapping.str.replace("\>", "overlap_")
overlapList = allSimpleUttDf.overlapping.str.split("overlap_", expand=True)
allSimpleUttDf["overlapping"] = overlapList[1]
allSimpleUttDf["overlapping"] = allSimpleUttDf["overlapping"].fillna("no_overlapping") # if a utterance doesn't have an overlapping it'll return "no_overlapping"

# now that the overlapping is separated ina column, we can delete the overlapping symbols and create
# a column without < and >
allSimpleUttDf["utterance_clean"] = allSimpleUttDf.utterance.str.replace("\<", "")
allSimpleUttDf["utterance_clean"] = allSimpleUttDf.utterance_clean.str.replace("\>", "")

# creating function to return the cancelled words
def findRetractings(string):
    one = "[a-zA-ZÀ-ÿũ]+ \[\/1\]"
    two = "[a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ \[\/2\]"
    three = "[a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ \[\/3\]"
    four = "[a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ \[\/4\]"
    five = "[a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ \[\/5\]"
    six = "[a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ \[\/6\]"
    seven = "[a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ \[\/7\]"
    eight = "[a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+  [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+\[\/8\]"
    nine = "[a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+  [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ [a-zA-ZÀ-ÿũ]+ \[\/9\]"
    fString = re.sub("\&", "X", string) # replace the hesitation symbol by an uppercase x
    if re.findall(two, fString):
        return re.findall(two, fString)
    elif re.findall(three, fString):
        return re.findall(three, fString)
    elif re.findall(four, fString):
        return re.findall(four, fString)
    elif re.findall(five, fString):
        return re.findall(five, fString)
    elif re.findall(six, fString):
        return re.findall(six, fString)
    elif re.findall(seven, fString):
        return re.findall(seven, fString)
    elif re.findall(eight, fString):
        return re.findall(eight, fString)
    elif re.findall(nine, fString):
        return re.findall(nine, fString)
    elif re.findall(one, fString):
        return re.findall(one, fString)
    else:
        return "no_retracting"

# applying the function
allSimpleUttDf['retractings'] = allSimpleUttDf['utterance_clean'].apply(lambda x: findRetractings(x))

# creating a function that deletes the cancelled words in the utterances WITHOUT the overlapping symbols
def deleteRetractings(string):
    one = "(\&|)[a-zA-ZÀ-ÿũ]+ \[\/1\]"
    two = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/2\]"
    three = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/3\]"
    four = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/4\]"
    five = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|\<|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/5\]"
    six = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/6\]"
    seven = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/7\]"
    eight = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/8\]"
    nine = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/9\]"
    final_string = re.sub(one, "",string)
    final_string = re.sub(two, "",final_string)
    final_string = re.sub(three, "",final_string)
    final_string = re.sub(four, "",final_string)
    final_string = re.sub(five, "",final_string)
    final_string = re.sub(six, "",final_string)
    final_string = re.sub(seven, "",final_string)
    final_string = re.sub(eight, "",final_string)
    final_string = re.sub(nine, "",final_string)
    final_string = re.sub("\s+", " ",final_string)
    final_string = re.sub("^ ", "",final_string)
    final_string = re.sub(" $", "",final_string)
    return final_string

# applying the function
allSimpleUttDf['utterance_clean'] = allSimpleUttDf['utterance_clean'].apply(lambda x: deleteRetractings(x))

# creating function to classify the type of paralinguistic sound
def classifyParalinguistic(string):
    if re.findall("(hhh$|hhh>$)", string): #in order to be relevant, it must be a complete utterance
        return "relevant_paralinguistic"
    elif re.findall("hhh(>|) [a-zA-ZÀ-ÿũ]+", string):
        return "not_relevant_paralinguistic"
    else:
        return "paralinguistic_not_present"

# applying the function
allSimpleUttDf["paralinguistic_sound"] = allSimpleUttDf['utterance'].apply(lambda x: classifyParalinguistic(x))

# reordering the columns and deleting the turn column (it's not necessary in this research)
reorder_allSimpleUttDf = allSimpleUttDf[["file", "acronym", "person_code_in_file", "utterance", "utterance_clean", "retractings",
                "overlapping", "paralinguistic_sound"]]

reorder_allSimpleUttDf[reorder_allSimpleUttDf.paralinguistic_sound =="relevant_paralinguistic"]

repr(reorder_allSimpleUttDf.iloc[151,4])



# # replacing the "hhh" in the 'not relevant' rows by nothing
# import numpy as np
# allSimpleUttDf['utterance_clean'] = np.where(allSimpleUttDf.paralinguistic_sound == 'not_relevant_paralinguistic', allSimpleUttDf['utterance_clean'].str.replace('hhh ', ''), allSimpleUttDf['utterance_clean'])

# repr(containValues.iloc[42,5])

# date = datetime.now().strftime('%Y-%m-%d')
# allSimpleUttDf.to_csv(textOutputPath+f'allSimpleUttDferancesDf_csv_{date}.csv')
# allSimpleUttDf.to_excel(textOutputPath+f'allSimpleUttDferancesDf_excel_{date}.xlsx')

"""## dump"""

# # creating a function that deletes the cancelled words in the utterances WITHOUT the overlapping symbols
# def deleteRetractings(string):
#     one = "(\&|)[a-zA-ZÀ-ÿũ]+ \[\/1\]"
#     two = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/2\]"
#     three = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/3\]"
#     four = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/4\]"
#     five = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|\<|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/5\]"
#     six = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/6\]"
#     seven = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/7\]"
#     eight = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/8\]"
#     nine = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/9\]"
#     final_string = re.sub(one, "",string)
#     final_string = re.sub(two, "",final_string)
#     final_string = re.sub(three, "",final_string)
#     final_string = re.sub(four, "",final_string)
#     final_string = re.sub(five, "",final_string)
#     final_string = re.sub(six, "",final_string)
#     final_string = re.sub(seven, "",final_string)
#     final_string = re.sub(eight, "",final_string)
#     final_string = re.sub(nine, "",final_string)
#     return final_string

# simpleTextList = []
# simpleTextDfList = [] #list of DFs
# for transcriptionPath in simpleTranscriptList:
#     with open(transcriptionPath, "r", encoding = "utf-8") as source:
#         transcription = source.read()
#         simpleTextList.append(transcription)
#         textDf = pd.DataFrame()
#         turnsList = transcription.split("\n")
#         newTurnsList = []
#     for turn in turnsList:
#         # turnClean = re.sub("^\n", "", turn)#deleting \n at the beggining of the line
#         turnClean = re.sub("\n", "", turn)#putting the end of turns as a utterance break
#         turnClean = re.sub("\+", "_utt", turnClean)#replacing + for //
#         turnClean = re.sub("//", "_utt", turnClean)#replacing + for //
#         personName = turnClean.split(":")[0][1:]
#         turnClean = re.sub("^\*[A-Z]{3}:", "", turnClean)#replacing + for //
#         newTurnsList.append([personName, turnClean])
#     textDf = pd.DataFrame(newTurnsList, columns=["person_abbreviation", "simple_turn"])
#     textDf = textDf[textDf.simple_turn !=' ']#deleting (possible) empty cells
#     textDf = textDf[textDf.simple_turn !='']
#     fileNameDirty = re.sub("\.txt", "", transcriptionPath)
#     fileName = re.sub('TextualCorpus\\b', '', fileNameDirty.split("/")[6])
#     fileName = re.sub("\\\\", "", fileName)
#     textDf["file"] = fileName
#     simpleTextDfList.append(textDf)

# allSimpleUttDferancesDf = pd.concat(simpleTextDfList) #creating one big DF
# allSimpleUttDferancesDf.simple_turn = allSimpleUttDferancesDf.simple_turn.str.replace("\[[0-9]\]", "")
# allSimpleUttDferancesDf.simple_turn = allSimpleUttDferancesDf.simple_turn.str.replace("\s+", " ")
# allSimpleUttDferancesDf["person_in_file"] = allSimpleUttDferancesDf.file + allSimpleUttDferancesDf['person_abbreviation'] #person's "id" in the file
# allSimpleUttDferancesDf = allSimpleUttDferancesDf[["file", "person_abbreviation", "person_in_file", "simple_turn"]] #reordering the columns
# allSimpleUttDferancesDf = allSimpleUttDferancesDf.reset_index()
# allSimpleUttDferancesDf = allSimpleUttDferancesDf[allSimpleUttDferancesDf.simple_turn !=' ']
# allSimpleUttDferancesDf = allSimpleUttDferancesDf[allSimpleUttDferancesDf.simple_turn !='']

# date = datetime.now().strftime('%Y-%m-%d')
# allSimpleUttDferancesDf.to_csv(textOutputPath+f'allSimpleUttDferancesDf_csv_{date}.csv')
# allSimpleUttDferancesDf.to_excel(textOutputPath+f'allSimpleUttDferancesDf_excel_{date}.xlsx')

# for turns in listTurnList:
    #     for turn in turns:
    #             turnClean = re.sub(" +", " ", turn)
    #             turnClean = re.sub("\+", "_utt", turnClean)
    #             turnClean = re.sub("\/\/", "_utt", turnClean)
    #             turnClean = re.sub("\n", "_utt", turnClean)
    #             # turnClean = re.sub(" $", "_utt", turnClean)
    #             turnClean = re.sub("_utt _utt", "_utt", turnClean)
    #             turnClean = re.sub("_utt_utt", "_utt", turnClean)
    #             # turnClean = re.sub(" $", "", turnClean)
    #             turnClean = re.sub("\*[A-Z]{3}:", "", turnClean)
    #             abb = turn.split(":")[0][1:]
    #             # abbreviationList.append(fileName+abb)
    #             newTurnList.append([abb, fileName, turnClean])

    # textDf = pd.DataFrame(newTurnList, columns=["personCode", "file", "turn"])

    # dfList.append(textDf)
    # allposUtt = pd.concat(dfList)
    # return allposUtt

# in case overlapping is important for you, run this line:
# allSimpleUttDf['overlapping'] = allSimpleUttDf['utterance_clean'].apply(lambda x: " ".join(re.findall('\<[\w+\s]+\>', x)))

# in case it isn't, run this one:
# we're deleting the interruption markup
# allSimpleUttDf['utterance_clean'] = allSimpleUttDf['utterance_clean'].apply(lambda x: re.sub("<", "", x))
# allSimpleUttDf['utterance_clean'] = allSimpleUttDf['utterance_clean'].apply(lambda x: re.sub(">", "", x))
# allSimpleUttDf['utterance_clean'] = allSimpleUttDf['utterance_clean'].apply(lambda x: re.sub("\s+", " ", x))

# # creating function to delete the cancelled words in retractings
# def deleteRetractings(string):
#         # creating regex to correspond to the retracting markup and the cancelled items
#         one = "(\&|)[a-zA-ZÀ-ÿũ]+ \[\/1\]"
#         one_over1 = "\<(\&|)[a-zA-ZÀ-ÿũ]+ \[\/1\] "
#         one_over2 = " (\&|)[a-zA-ZÀ-ÿũ]+\> \[\/1\]"
#         one_over3 = "\<(\&|)[a-zA-ZÀ-ÿũ]+\> \[\/1\]"
#         # ====
#         two = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/2\]"
#         two_over1 = "\<(\&|)[a-zA-ZÀ-ÿũ]+\> (\&|)[a-zA-ZÀ-ÿũ]+ \[\/2\]"
#         two_over11 = "(\&|)[a-zA-ZÀ-ÿũ]+ \<(\&|)[a-zA-ZÀ-ÿũ]+\> \[\/2\]"
#         two_over2 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+\> \[\/2\]"
#         two_over3 = "\<(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+\> \[\/2\]"
#         # ====
#         three = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/3\]" # take it all
#         three_over1 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+\> \[\/3\]" #>
#         three_over2 = "(\&|)[a-zA-ZÀ-ÿũ]+\> (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/3\]" #>
#         three_over3 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+\> (\&|)[a-zA-ZÀ-ÿũ]+ \[\/3\]" #>
#         three_over4 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+\> \[\/3\]" #>
#         three_over5 = "\<(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/3\]" #<
#         three_over6 = "(\&|)[a-zA-ZÀ-ÿũ]+ \<(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/3\]" #<
#         three_over7 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \<(\&|)[a-zA-ZÀ-ÿũ]+ \[\/3\]" #<
#         three_over8 = "\<(\&|)[a-zA-ZÀ-ÿũ]+\> (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/3\]" #take it all
#         three_over9 = "(\&|)[a-zA-ZÀ-ÿũ]+ \<(\&|)[a-zA-ZÀ-ÿũ]+\> (\&|)[a-zA-ZÀ-ÿũ]+ \[\/3\]" #take it all
#         three_over10 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \<(\&|)[a-zA-ZÀ-ÿũ]+ \[\/3\]\>" #take it all
#         # ====
#         four = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/4\]"
#         four_over1 = "\<(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+\> \[\/4\]"
#         four_over2 = "(\&|)[a-zA-ZÀ-ÿũ]+ \<(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/4\]"
#         four_over3 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \<(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/4\]"
#         four_over4 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \<(\&|)[a-zA-ZÀ-ÿũ]+ \[\/4\]"
#         four_over5 = "(\&|)[a-zA-ZÀ-ÿũ]+\> (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/4\]"
#         four_over6 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+\> (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/4\]"
#         four_over7 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+\> (\&|)[a-zA-ZÀ-ÿũ]+ \[\/4\]"
#         four_over8 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+\> \[\/4\]"

#         # ====
#         five = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|\<|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/5\]"
#         five_over1 = "\<(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+\> \[\/5\]"
#         five_over2 = "\<(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/5\]"
#         five_over3 = "(\&|)[a-zA-ZÀ-ÿũ]+ \<(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/5\]"
#         five_over4 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \<(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/5\]"
#         five_over5 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \<(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/5\]"
#         five_over6 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \<(\&|)[a-zA-ZÀ-ÿũ]+ \[\/5\]"
#         five_over7 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+\> \[\/5\]"
#         five_over8 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+\> (\&|)[a-zA-ZÀ-ÿũ]+ \[\/5\]"
#         five_over9 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+\> (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/5\]"
#         five_over10 = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+\> (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/5\]"
#         five_over11 = "(\&|)[a-zA-ZÀ-ÿũ]+\> (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/5\]"
#         # ====
#         six = "(\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ (\&|)[a-zA-ZÀ-ÿũ]+ \[\/6\]"
#         six_over1 = "(\<|)(\&|)[a-zA-ZÀ-ÿũ]+ (\<|)(\&|)[a-zA-ZÀ-ÿũ]+ (\<|)(\&|)[a-zA-ZÀ-ÿũ]+ (\<|)(\&|)[a-zA-ZÀ-ÿũ]+ (\<|)(\&|)[a-zA-ZÀ-ÿũ]+ (\<|)(\&|)[a-zA-ZÀ-ÿũ]+"
#         six_over2 = "(\&|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|)[a-zA-ZÀ-ÿũ]+"

#         # ====
#         seven = "(\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) \[\/7\]"
#         eight = "(\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) \[\/8\]"
#         nine = "(\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) (\&|\<|)[a-zA-ZÀ-ÿũ]+(\>|) \[\/9\]"

#         # replacing the regex above by nothing
#         final_string = re.sub(one_over1, "<",string)
#         final_string = re.sub(one_over2, ">",final_string)
#         final_string = re.sub(one_over3, "",final_string)
#         final_string = re.sub(two_over1, "",final_string)
#         final_string = re.sub(two_over11, "",final_string)
#         final_string = re.sub(two_over2, ">",final_string)
#         final_string = re.sub(two_over3, "",final_string)
#         final_string = re.sub(one,"", final_string)
#         final_string = re.sub(two, "",final_string)
#         # =====
#         final_string = re.sub(three, "", final_string)
#         final_string = re.sub(three_over1, ">", final_string)
#         final_string = re.sub(three_over2, ">", final_string)
#         final_string = re.sub(three_over3, ">", final_string)
#         final_string = re.sub(three_over4, ">", final_string)
#         final_string = re.sub(three_over5, "<", final_string)
#         final_string = re.sub(three_over6, "<", final_string)
#         final_string = re.sub(three_over7, "<", final_string)
#         final_string = re.sub(three_over8, "", final_string)
#         final_string = re.sub(three_over9, "", final_string)
#         final_string = re.sub(three_over10, "", final_string)
#         # =====
#         final_string = re.sub(four, "", final_string)
#         final_string = re.sub(four_over1, "", final_string)
#         final_string = re.sub(four_over2, "<", final_string)
#         final_string = re.sub(four_over3, "<", final_string)
#         final_string = re.sub(four_over4, "<", final_string)
#         final_string = re.sub(four_over5, ">", final_string)
#         final_string = re.sub(four_over6, ">", final_string)
#         final_string = re.sub(four_over7, ">", final_string)
#         final_string = re.sub(four_over8, ">", final_string)
#         # =====
#         final_string = re.sub(five, "", final_string)
#         final_string = re.sub(five_over1, "", final_string)
#         final_string = re.sub(five_over2, "<", final_string)
#         final_string = re.sub(five_over3, "<", final_string)
#         final_string = re.sub(five_over4, "<", final_string)
#         final_string = re.sub(five_over5, "<", final_string)
#         final_string = re.sub(five_over6, "<", final_string)
#         final_string = re.sub(five_over7, ">", final_string)
#         final_string = re.sub(five_over8, ">", final_string)
#         final_string = re.sub(five_over9, ">", final_string)
#         final_string = re.sub(five_over10, ">", final_string)
#         final_string = re.sub(five_over11, ">", final_string)
#         # =====
#         final_string = re.sub(six, "", final_string)
#         final_string = re.sub(six_over1, "<", final_string)
#         final_string = re.sub(six_over2, ">", final_string)
#         final_string = re.sub(seven, "", final_string)
#         final_string = re.sub(eight, "", final_string)
#         final_string = re.sub(nine, "", final_string)
#         final_string = re.sub("< ", "<", final_string)
#         final_string = re.sub(" >", ">", final_string)
#         final_string = re.sub("<>", "", final_string)
#         final_string = re.sub("\s+", " ", final_string) # the above replacements may cause extra whitespaces, here we're replacing extra whitespaces by only one
#         return final_string
# Sociolinguistic profiling of Brazilian Portuguese Speakers

## Starting out 📜

This repository houses a computational research project focused on sociolect (dis)similarity recognition and speaker profiling using speech transcriptions. Positioned within the emerging field of Computational Sociolinguistics, the study employs three methods: Variation-Based Distance and Similarity Modeling (VADIS), Mann-Whitney (non-parametric modeling), and Poisson and Negative binomial with Estimated Marginal Means (EMM) and Compact Letter Display (CLD) (parametric modeling). Extracted from a Brazilian Portuguese spontaneous speech corpus, the data includes transcriptions and metadata information. The research evaluates twelve linguistic variables and reveals the limitations of VADIS, while showcasing the effectiveness of non-parametric and parametric models in sociolect identification. The implemented Poisson and Negative binomial models, alongside EMM and CLD, demonstrate their utility in linguistically profiling speakers through speech transcriptions. The study contributes to advancing sociolinguistics by integrating computational methods into the analysis of Brazilian Portuguese sociolects, shedding light on intricate relations between society and language.

## Data 💡
- Data source: [C-ORAL BRASIL I ](https://www.c-oral-brasil.org/corpora_para_download.php)
- allPeopleInfoDf: all speakers' metadata
- posTagUtteranceWithInfo: speakers' utterances and extracted variables
- metadataWithTranscription: extracted variables, utterances and speakers' metadata

## Using the codes 🎉

The Python🐍 codes were used to read, process, clean and organize the data so that it could go over the models. We suggest you to use them in the following order:
1. reading_postagged_files -> read and preprocess the transcription files
2. reading_header_files -> read and preprocess the metadata files
3. extracting_info_from_postagged_texts -> extract linguistic variables from the output dataframe in reading_postagged_files
4. combining_metadata_postagged -> merge the dataframes from extracting_info_from_postagged_texts and reading_header_files
5. extracting_simple_metrics -> extract simple metrics from the output from combining_metadata_postagged

The R codes can be used in whatever order you may prefer!

## Final paper 🖋️
TO BE PUBLISHED

## Gratitude note 🦄

I would like to thank my wonderful advisors Dr. Heliana Mello and Dr. Crysttian Paixão!

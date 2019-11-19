
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rvent\_app

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.2.0.9000-blue.svg)](https://github.com/https://github.com/c1au6i0/rvent_app)
<!-- badges: end -->

The rvent\_app is a shiny application developed to import, bin and
summarized session files obtained with the [SCIREQ
pletysmograph](https://www.scireq.com/). Under the hood it uses
functions of tje [rvent package](https://github.com/c1au6i0/rvent).

## App

A tutorial is available within the rvent\_app. The rvent\_app is hosted
here: <https://rden.shinyapps.io/rvent_app/>

### Accepted files and recording

The rvent\_app extract some of the metadata from the subject name and
comments provide in the output file. Thus, it is important to follow
some rules for recording sessions.

**Subject name**

*Subject name* needs to contain a numeric ID (rest of the name will be
discarded) followed by a drug name. These are all examples of accepted
formats for subject 1 receiving 3 mg/kg of milk:

    1 milk 3 mg/kg # prefered
    1_milk_3_mg/kg
    1 milk
    1 milk ignoredinfo

These are examples of not accepted formats:

    milk 1
    milk rat1

**Comments**

*Comments* are used to identify the injection time. When one or more
subjects receive an injection, use the SCIREQ software to add a comment
in the form ID DRUG DOSE UNIT. For example, these are the accepted
formats for a session in which subjects 1, 2 and 3 received 3 mg/kg of
milk:

    1 2 3 milk 3 mg/kg # prefered
    rat 1 and 2 and 3 milk 3 mg/kg
    rat1_2_3_milk 3 mg/kg
    rat1 2 3 milk

**If dose or unit are not inserted**, the rvent\_app will ask you to
fill that info.

Note that subject are expected to get one injection per session.

### Example file

This is an example of a recording file (containing randomly generated
data).

<iframe src="https://onedrive.live.com/embed?cid=BCC43C613C51DA6D&amp;resid=BCC43C613C51DA6D%21141111&amp;authkey=AFBy2KArg1drDck" width="98" height="120" frameborder="0" scrolling="no">

</iframe>

# A-B-Testing
A/B testing on Udacity's experiment named: “Free Trial” Screener. 

In an experiment, Udacity tested a change where if the student clicked “start free trial”, they were asked how much time they had available to devote to the course.If the student indicated 5 or more hours per week, they would be taken through the checkout process as usual. If they indicated fewer than 5 hours per week, a message would appear indicating that Udacity courses usually require a greater time commitment for successful completion.

The goal with this popup was that this might set clearer expectations for students upfront, thus reducing the number of frustrated students who left the free trial because they didn’t have enough time.

# Goal of the project
1. Identify features impacting the enrollments.
2. Investigate if the new popup is increasing enrollments.

# Dataset Information
control_data.csv (37 records with 5 attributes)
experiment_data.csv (37 records with 5 attributes)

# Attribute Information
Name | Description
-----|-------------
Date|Date;
Pageviews|Number of unique cookies to view the course overview page that day;
Clicks|No. of clicks on "Start Free trial";
Enrollments|Number of user-ids to enroll in the free trial that day;
Payments|Number of user-ids who who enrolled on that day to remain enrolled for 14 days and thus make a payment;



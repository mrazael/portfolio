import json
import re
from datetime import datetime

class BigData:

    def __init__(self, filename = 'RC_2015-01'):

        self.filename = filename
        self.subreddits = {}
        self.users = {}
        self.totalposts = 0
        self.totalkarma = 0

    def json_to_csv(self):

        n = 0
        k = 1000000

        with open('reddit.csv', 'w', newline = '') as reddit:
            with open(self.filename, 'r', encoding = 'utf-8') as file:
                reddit.write('\t'.join(["created_utc", "author", "body", "score", "subreddit" + '\n']))
                for line in file:
                    data = json.loads(line)
                    data['body'] = data['body'].replace('\t', '').replace('\r', '').replace('\n', '').lower()
                    if data['author'] != "[deleted]" and data['body'] != "[deleted]":
                        if re.findall(r'([Aa][Uu][Tt][O0o])|([Bb][O0o][Tt])|([Mm][O0o][Dd])|([Tt]ranscriber)', data['author']):
                            continue

                        else:

                            if data['subreddit'] in self.subreddits:
                                self.subreddits[data['subreddit']] = (self.subreddits[data['subreddit']][0]+1, self.subreddits[data['subreddit']][1] + int(data['score']))
                            if data['author'] in self.users:
                                self.users[data['author']] = (self.users[data['author']][0]+1, self.users[data['author']][1] + int(data['score']))
                            if data['subreddit'] not in self.subreddits:
                                self.subreddits[data['subreddit']] = (1, int(data['score']))
                            if data['author'] not in self.users:
                                self.users[data['author']] = (1, int(data['score']))

                            self.totalposts += 1
                            self.totalkarma += data['score']

                            data['created_utc'] = datetime.utcfromtimestamp(int(data['created_utc'])).strftime('%Y-%m-%d %H:%M:%S')
                            reddit.write('\t'.join([data['created_utc'], data['author'], data['body'], str(data['score']), data['subreddit'] + '\n']))

                            n += 1
                            if k == n:
                                print(f'Processed {n} lines.')
                                k += 1000000

        print("Finished!")

    def results(self):
        print(f"Processed {n} lines. Here are the summary statistics:")

        topsubs = [(k, v) for k, v in sorted(self.subreddits.items(), key=lambda item: item[1], reverse = True)]
        topusers = [(k, v) for k, v in sorted(self.users.items(), key=lambda item: item[1], reverse = True)]
        topkarma = [(k, v) for k, v in sorted(self.users.items(), key=lambda item: item[1][1], reverse = True)]

        groups = {"Superusers": (0, 0), "Contributors": (0, 0), "Lurkers": (0, 0)}

        subposts = 0
        subkarma = 0

        for i in range(16):
            subposts += topsubs[i][1][0]
            subkarma += topsubs[i][1][1]

        print("")
        print("Subreddits in total:", len(topsubs))
        print(f'Top 16 subreddits account for {format(subposts/self.totalposts*100, ".2f")}% of all posts and {format(subkarma/self.totalkarma*100, ".2f")}% of total karma')

        for i in range(len(topusers)):
            if i < round(len(topusers)*0.01):
                groups["Superusers"] = (groups["Superusers"][0] + topusers[i][1][0], groups["Superusers"][1] + topusers[i][1][1])

            elif i < round(len(topusers)*0.1):
                groups["Contributors"] = (groups["Contributors"][0] + topusers[i][1][0], groups["Contributors"][1] + topusers[i][1][1])

            else:
                groups["Lurkers"] = (groups["Lurkers"][0] + topusers[i][1][0], groups["Lurkers"][1] + topusers[i][1][1])

        print("Users in total:", len(topusers))
        print("")
        for index in groups:
            print(f'{index} have in total made {groups[index][0]} posts ({format(groups[index][0]/totalposts*100, ".2f")}% of all posts) and accumulated {groups[index][1]} karma ({format(groups[index][1]/totalkarma*100, ".2f")}% of total karma)')
        print("")
        ######################

        sampletopsubs = []

        print('================Top subreddits=========================')
        for i in range(16):
            sampletopsubs.append(topsubs[i][0])
            if i < 9:
                print(f'{"0" + str(i+1):02} {topsubs[i][0]: ^40} {str(topsubs[i][1][0]): ^5} {str(topsubs[i][1][1]): ^5}')
            else:
                print(f'{str(i+1)} {topsubs[i][0]: ^40} {str(topsubs[i][1][0]): ^5} {str(topsubs[i][1][1]): ^5}')

        print("")

        print('===================Top users==========================')
        for i in range(16):
            if i < 9:
                print(f'{"0" + str(i+1):02} {topusers[i][0]: ^40} {str(topusers[i][1][0]): ^5} {str(topusers[i][1][1]): ^5}')
            else:
                print(f'{str(i+1)} {topusers[i][0]: ^40} {str(topusers[i][1][0]): ^5} {str(topusers[i][1][1]): ^5}')

        ###################
        print("")
        # print("Systematic sample of 100 subreddits:")
        n = 0

        samplesubs = []

        while n <= len(topsubs):
            if topsubs[n][1][0] >= 1000:
                samplesubs.append(topsubs[n][0])
            n += round(len(topsubs)*0.001)

        print(samplesubs)
        print("")
        print(sampletopsubs)

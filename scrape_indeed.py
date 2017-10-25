from bs4 import BeautifulSoup
import requests
import time


def download_page(query, location, page):
    """
    Given strings representing the query and the location
    and an integer representing the page number, returns
    the contents of the specified page on an Indeed search.
    Doesn't check that the page makes sense
    """
    url = 'https://www.indeed.com/jobs?q={0}&l={1}{2}&sort=date'.format(
        query.replace(' ', '+'),
        location.replace(' ', '+'),
        '' if page == 1 else '&start='+str(10*(page-1)))
    return requests.get(url).text


def is_last_page(soup):
    """
    returns whether or not there is a 'next' button
    """
    arrows = [d.text for d in soup.findAll('span', class_='np')]
    return len(arrows) > 0 and "Next" not in arrows[-1]


def get_job_summaries(query, location, start_page, end_page):
    with open('data/summaries.txt', 'wb') as outfile:
        for page in range(start_page, end_page+1):
            soup = BeautifulSoup(download_page(query, location, page), 'html.parser')
            for div in soup.findAll('div', class_='result'):
                summary = re.sub('[\t\n]', ' ', div.find('span', class_='summary').text)
                experience = div.find('span', class_='experienceList')
                if experience:
                    experience = experience.text
                else:
                    experience = " "
                ## have to use dictionary because 'data-tn-element' is not a valid var name
                link = div.find('a', attrs={'data-tn-element': 'jobTitle'})
                job_url = 'http://www.indeed.com' + link['href']
                title = link['title']
                company = re.sub('[\t\n]', ' ', div.find('span', class_='company').text)
                line = summary + '\t' + experience + '\t' + job_url + '\t' + title + '\t' + company + '\n'
                outfile.write(line.encode('utf-8', 'ignore'))
            if is_last_page(soup):
                break
            time.sleep(1)

get_job_summaries("data engineer", "Berkeley, CA", 1, 5)


from selenium import webdriver
from selenium.webdriver.support.ui import Select
from bs4 import BeautifulSoup, NavigableString
import requests
import time

def gs25_crawl():

    print('gs25 start!')
    txt_File.write('gs25\n')

    options = webdriver.ChromeOptions()
    options.add_argument('--ignore-certificate-errors')
    options.add_argument('--incognito')
    driver = webdriver.Chrome("./chromedriver", options=options)

    initial_link = "https://gs25.uplussave.com/prtn/gs25/shopSearchPop.mhp#n"
    driver.get(initial_link)

    driver.find_element_by_class_name('btn_location').click()

    button1 = driver.find_elements_by_class_name('select_wrap')[0]
    button1.click()
    time.sleep(1)
    category1_len = len(driver.find_element_by_class_name('select_list_wrap').find_elements_by_tag_name('a'))
    button1.click()
    
    for index1 in range(category1_len-1):
        try:
            button1.click()
            time.sleep(0.5)
            driver.find_element_by_class_name('select_list_wrap').find_elements_by_tag_name('a')[index1+1].click()
            time.sleep(0.5)
            
            button2 = driver.find_elements_by_class_name('select_wrap')[1]
            button2.click()
            time.sleep(2)
            category2_len = len(driver.find_element_by_class_name('select_list_wrap').find_elements_by_tag_name('a'))
            button2.click()

            for index2 in range(category2_len-1):
                button2.click()
                time.sleep(0.5)
                driver.find_element_by_class_name('select_list_wrap').find_elements_by_tag_name('a')[index2+1].click()
                time.sleep(0.5)
                
                text = driver.find_element_by_class_name('adrs_info').text
                text = text.replace(' > ', ',')
                text = text.replace(' 검색결과 총 ', ',')
                text = text.replace('건 검색되었습니다.', '')
                print(text)
                txt_File.write(text+'\n')

        except Exception as e:
            print(str(e))
        
    print('gs25 end!')
    driver.close()

        
def cu_crawl():

    print('cu start!')
    txt_File.write('cu\n')

    options = webdriver.ChromeOptions()
    options.add_argument('--ignore-certificate-errors')
    options.add_argument('--incognito')
    driver = webdriver.Chrome("./chromedriver", options=options)

    initial_link = "https://cu.bgfretail.com/store/list.do?category=store"
    driver.get(initial_link)
    time.sleep(3)

    select1 = Select(driver.find_element_by_id('sido'))
    for option1 in select1.options[1:]:
        try:
            select1.select_by_visible_text(option1.text)
            time.sleep(0.5)
            
            select2 = Select(driver.find_element_by_id('Gugun'))
            for option2 in select2.options[1:]:
                select2.select_by_visible_text(option2.text)
                time.sleep(0.5)

                driver.find_element_by_class_name('btn_wrap').find_element_by_tag_name('input').click()
                time.sleep(1)

                try:
                    driver.find_element_by_class_name('pagination').find_elements_by_class_name('Image')[3].click()
                except:
                    driver.find_element_by_class_name('pagination').find_elements_by_tag_name('a')[-1].click()

                time.sleep(1)

                page = int(driver.find_element_by_class_name('Current').text)
                additional = len(driver.find_element_by_class_name('detail_store').find_element_by_tag_name('tbody').find_elements_by_tag_name('tr'))
                print(option1.text+","+option2.text+","+str((page-1)*5+additional))
                txt_File.write(option1.text+","+option2.text+","+str((page-1)*5+additional)+'\n')
        
        except Exception as e:
            print(str(e))

    print('cu end!')
    driver.close()

        
def seven_crawl():

    print('7_11 start!')
    txt_File.write('7_11\n')

    options = webdriver.ChromeOptions()
    options.add_argument('--ignore-certificate-errors')
    options.add_argument('--incognito')
    driver = webdriver.Chrome("./chromedriver", options=options)

    initial_link = "http://www.7-eleven.co.kr/"
    driver.get(initial_link)
    time.sleep(3)

    driver.find_element_by_class_name('head_util').click()
    time.sleep(3)

    option1_len = len(Select(driver.find_element_by_id('storeLaySido')).options)

    for index1 in range(option1_len-1):
        try:
            select1 = Select(driver.find_element_by_id('storeLaySido'))
            text1 = select1.options[index1+1].text
            select1.select_by_index(index1+1)
            time.sleep(0.5)
            
            option2_len = len(Select(driver.find_element_by_id('storeLayGu')).options)
            for index2 in range(option2_len-1):
                select2 = Select(driver.find_element_by_id('storeLayGu'))
                text2 = select2.options[index2+1].text
                select2.select_by_index(index2+1)
                time.sleep(0.5)

                driver.find_element_by_id('storeButton1').click()
                time.sleep(3)
                
                num = len(driver.find_element_by_class_name('list_stroe').find_elements_by_tag_name('li'))

                print(text1+","+text2+","+str(num))
                txt_File.write(text1+","+text2+","+str(num)+'\n')
        
        except Exception as e:
            print(str(e))

    print('7_11 end!')
    driver.close()
        

def emart_crawl():

    print('emart24 start!')
    txt_File.write('emart\n')

    options = webdriver.ChromeOptions()
    options.add_argument('--ignore-certificate-errors')
    options.add_argument('--incognito')
    driver = webdriver.Chrome("./chromedriver", options=options)

    initial_link = "https://www.emart24.co.kr/introduce2/findBranch.asp"
    driver.get(initial_link)
    time.sleep(3)

    option1_len = len(Select(driver.find_element_by_id('stplacesido')).options)

    for index1 in range(option1_len-1):
        try:
            select1 = Select(driver.find_element_by_id('stplacesido'))
            text1 = select1.options[index1+1].text
            select1.select_by_index(index1+1)
            time.sleep(0.5)
            
            option2_len = len(Select(driver.find_element_by_id('stplacegugun')).options)
            for index2 in range(option2_len-1):
                select2 = Select(driver.find_element_by_id('stplacegugun'))
                text2 = select2.options[index2+1].text
                select2.select_by_index(index2+1)
                time.sleep(0.5)

                driver.find_element_by_class_name('find_srchArea').find_elements_by_tag_name('input')[1].click()
                time.sleep(3)

                driver.find_element_by_class_name('paging').find_elements_by_class_name('bgNone')[3].click()
                time.sleep(1)

                page = int(driver.find_element_by_class_name('paging').find_element_by_class_name('first').text)
                additional = len(driver.find_element_by_class_name('find_listArea').find_element_by_tag_name('tbody').find_elements_by_tag_name('tr'))
                print(text1+","+text2+","+str((page-1)*5+additional))
                txt_File.write(text1+","+text2+","+str((page-1)*5+additional)+'\n')
        
        except Exception as e:
            print(str(e))

    print('emart24 end!')
    driver.close()


txt_File = open('test_result.txt', 'a', encoding='utf-8')


# gs25_crawl()
cu_crawl()
seven_crawl()
emart_crawl()
txt_File.close()
print("finish!")

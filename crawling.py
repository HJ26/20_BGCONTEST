
def craw():
	from urllib.request import urlopen
	from bs4 import BeautifulSoup

	html = urlopen("https://search.naver.com/search.naver?where=nexearch&sm=top_hty&fbm=1&ie=utf8&query=%EC%BD%94%EB%A1%9C%EB%82%98")
	soup = BeautifulSoup(html,"html.parser")

	p = soup.find('li',class_='info_01')	

	p2 = p.find('p',class_='info_num')
	info_num = p2.get_text()
	
	p3 = p.find('em',class_='info_variation')
	info_v = p3.get_text() 
	
	return info_num, info_v;

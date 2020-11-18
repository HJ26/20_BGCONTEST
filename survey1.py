import crawling

result = 0

print("두개의 설문지가 준비되어 있습니다.",
	"\n어느 설문지를 선택하시겠습니까?")

while True:
	x = input("1: 바이러스 노출정도 문답, 2: COVID-19 관련 지식 문답(해당 번호를 입력하세요.) :")
	if x == "1":
		print("바이러스 노출 정도 문답을 선택하셨습니다.")
		print("최근 14일을 기준으로 질문에 답해주시기 바랍니다.")		

		a1 = input("Q1 : 대중시설(카페, 음식점, 주점 등)에 방문하였습니까?(Y,N)\n")
		a2 = input("Q2 : 사람이 있는 실내에서 마스크를 벗은 적이 있습니까?(Y,N)\n")
		a3 = input("Q3 : 음식물 섭취 전 손을 세정 하였습니까?(Y,N)\n")
		a4 = input("Q4 : 호흡기를 만지기 전 손을 세정 하였습니까?(Y,N)\n")	
		a5 = input("Q5 : 출퇴근(오전6~8시, 오후5~8시) 시간 대중교통(지하철, 버스)를 이용하였습니까?(Y,N)\n")
		a6 = input("Q6 : 외부에서 자택에 복귀한 후 핸드폰을 소독하였습니까?(Y,N)\n")
		a7 = input("Q7 : 식사 중 다른 사람과 일정거리를 두고 식사를 하였습니까?(Y,N)\n")
		a8 = input("Q8 : 공용 컵이나 식기가 아닌 개인 물품(개인 컵, 개인 식기)을 사용하였습니까?(Y,N)\n")
		a9 = input("Q9 : 다른사람과의 신체접촉(악수 등) 후 손을 세정 하였습니까?(Y,N)\n")
		a10 = input("Q10 : 노래방,PC방 같은 제한시설을 이용한적 있습니까?(Y,N)\n")

		
		print('\n답변하신 내용입니다.:',a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)

	
		if a1.upper() == "N":
			result += 1
		if a2.upper() == "N":
			result += 1
		if a3.upper() == "Y":
			result += 1
		if a4.upper() == "Y":
			result += 1
		if a5.upper() == "N":
			result += 1
		if a6.upper() == 'Y':
			result += 1
		if a7.upper() == "Y":
			result += 1
		if a8.upper() == "Y":
			result += 1
		if a9.upper() == "Y":
			result += 1
		if a10.upper() == "N":
			result += 1
		
		print('\n점수: ',result)
		if result < 2:
			print("당신의 마스크는 KF 20 : 구멍뚫린 마스크입니다.",
				"\n!위험! 예방에 주의가 필요합니다!")
		elif result < 4:
			print("당신의 마스크는 KF 40 :  바람막이 마스크입니다.",
				"\n바람은 막는 당신, 바이러스도 막아낼 수 있도록 노력해 보아요!")
		elif result < 6:
			print("당신의 마스크는 KF 60 : 그럭저럭 마스크입니다.",
				"\n노력하는 당신! 하지만 완벽까지 갈 길이 멀어요!")
		elif result < 8:
			print("당신의 마스크는 KF 80 : 믿을만한 마스크입니다.",
				"\n칭찬해요! 조금만 더 주의를 기울여보아요!")
		else:
			print("당신의 마스크는 KF 99 : 철통보안 마스크입니다.",
				"\n완벽해요! 당신의 마스크를 응원합니다!")
	
		a, b = crawling.craw()
		print("\n현재 국내 누적 확진자 수: ",a,"\n추가 확진자 수: ",b)
	
 
		break;
	

	elif x == "2":
		print("관련 지식 문답을 선택하셨습니다.")
		

		a1 = input("Q1 : 올바른 손씻기에 알고 있으며, 30초 이상 손을 씻는다.(Y/N)\n")
		a2 = input("Q2 : 38℃ 이상 고열이 지속되거나 증상이 심해질 경우 전화해야할 콜센터 번호는? (4글자 숫자)\n")
		a3 = input("Q3 : 가까운 선별 진료소의 위치를 알고 있다.(Y/N)\n")
		a4 = input("Q4 : 기침이나 재채기를 할 때 손으로 입과 코를 가려야 한다(o,x)\n")
		a5 = input("Q5 : 자신이 확진자의 가족이거나 접촉의 위험이 있는 경우 확진자와 최종접촉일 다음날부터 몇 일 동안 자가 격리를 해야 하는가?(2글자 숫자)\n")
		a6 = input("Q6 : 자가진단의 항목으로는 발열(37.5도 이상), 기침, 인후통/목아픔, 호흡곤란, 숨가쁨이 있다(o,x)\n")
		a7 = input("Q7 : COVID-19에 감염된 사람은 모두 증상이 나타난다(o,x)\n")
		a8 = input("Q8 : COVID-19는 눈으로도 감염될 수 있다.(o,x)\n")
		a9 = input("Q9 : 식품의약품안전처에서 ‘의약외품’으로 허가된 마스크는 모두 사용 권고 마스크이다.(o,x)\n")
		a10 = input("Q10 : 코로나19 환자를 돌보는 경우 수술용 마스크 착용이 필수이다.(o,x)\n")


		print('\n답변하신 내용입니다. : ',a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)


		if a1.upper() == "Y":
			result += 1
		else : print("Q1 review : 올바른 손씻기는 흐르는 물에 30초 이상 씻는 것을 권고하고 있으며, 자세한 사항은 다음 영상을 참고하세요 : https://www.youtube.com/watch?v=Sd9mHT9tOHU")
		
		if a2 == "1339":
			result += 1
		else : print("Q2 review : 증상이 의심될 경우 콜센터 1339를 기억해주세요.")
		
		if a3.upper() == "Y":
			result += 1
		else : print("Q3 review : 가까운 선별진료소 위치 검색 url : //www.mohw.go.kr/react/popup_200128_3.html")		

		if a4.upper() == "X":
			result += 1
		else : print("Q4 review : 기침이나 재채기를 할 때는 손대신 손수건이나 옷소매로 가리고 하시는 것을 권유드립니다.")
		if a5 == "14":
			result += 1
		else: print("Q5 review : 확진자와의 접촉의 위험이 있는 경우 잠복기간인 14일 자가 격리가 필요합니다.")
		if a6.upper() == 'O':
			result += 1
		else : print("Q6 review : 자가진단의 항목으로는 발열(37.5도 이상), 기침, 인후통/목아픔, 호흡곤란, 숨가쁨이 있습니다.")
		if a7.upper() == "X":
			result += 1
		else : print("Q7 review : 감염이 되어도 증상이 나타나지 않는 '무증상자'가 있습니다.")
		if a8.upper() == "O":
			result += 1
		else : print("Q8 review : COVID-19는 눈으로도 감염될 수 있습니다.")
		if a9.upper() == "X":
			result += 1
		else : print("Q9 review : 식품의약품안전처에서 ‘의약외품’으로 허가된 마스크 중 '밸브형 마스크'는 권고 마스크가 아닙니다.")
		if a10.upper() == "X":
			result += 1
		else : print("코로나19 환자를 돌보는 경우 수술용 부직포 마스크가 아닌 kf94 보견용 마스크가 필수입니다.")

		print('\n점수: ',result)
		if result < 2:
			print("당신의 마스크는 KF 20 : 구멍뚫린 마스크입니다.",
				"\n!위험! 예방에 주의가 필요합니다!")
		elif result < 4:
			print("당신의 마스크는 KF 40 : 바람막이 마스크입니다.",
				"\n바람은 막는 당신, 바이러스도 막아낼 수 있도록 노력해 보아요!")
		elif result < 6:
			print("당신의 마스크는 KF 60 : 그럭저럭 마스크입니다.",
				"\n노력하는 당신! 하지만 완벽까지 갈 길이 멀어요!")
		elif result < 8:
			print("당신의 마스크는 KF 80 : 믿을만한 마스크입니다.",
				"\n칭찬해요! 조금만 더 주의를 기울여 보아요!")
		else:
			print("당신의 마스크는 KF 99 : 철통보안 마스크입니다.",
				"\n완벽해요! 당신의 마스크를 응원합니다!")


		a, b = crawling.craw()
		print("\n현재 국내 누적 확진자 수: ",a,"\n추가 확진자 수: ",b)

		break;


	else: 
		print('잘못된 입력 입니다. 다시 입력해 주세요')
		continue;

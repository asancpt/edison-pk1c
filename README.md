# one comp simulation - Edison Project


시뮬레이션을 할 수 있는 Edison 앱입니다.

## Data files

- `1CompIV-NCA-result.csv`, `1CompIV-SimConc.csv`
- `FullCov-1coma.csv`, `FullCov2-1coma.csv`

# Correspondence

## simulation

1 comp, IV or Oral, linear elimination with macro constant simulation routine입니다.
 
추후 R package로도 개발해야 하겠지만, EDISON에도 올려야 합니다.
 
EDISON에는 각각 올리고,
Package는 한꺼번에 만들 생각입니다.
 
첨부파일들을 수행해보세요.
월요일 설명하지요.
 
Oral의 경우 BE simulation하는 부분도 있는데,
마지막에 BE 처리를 해보려면 며칠 전에 보낸 BELIB2.R이 있어야 합니다.
 
## simulated csv

1CompIV-NCA-result.csv
7 KB

1CompIV-SimConc.csv
3 KB

첨부파일이 R로 simulation한 자료를 ncar로 NCA한 것입니다.
Bolus dosing이고, dose는 100000 (십만)입니다.

C0 값이 이상하게 나와서, (나름 WinNonlin과 같은 알고리듬을 썼다고 했는데)
WinNonlin과 NCA 수행 결과를 비교해주세요.

나한테는 지금 WinNonlin이 설치되어 있지 않아서 보냅니다.

월요일쯤, PK Simulation에 대해 설명할 시간이 있기를 바라고,
Compartment model 마다 하나씩 EDISON project를 하나씩 만듭시다.

Simulation 관련해서 설명할 R 파일은 헷갈리지 않기 위해 다음 메일로 보냅니다.

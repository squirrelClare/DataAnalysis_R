rfunctions = open('out.txt',mode='r')
functions = []
for line in rfunctions.readlines():
    functions.extend(line.split(','))
    
print(len(functions))
rout = open('D://r.functions.templetes.xml','w')
rfuns = open('D://r.fun.txt','w')
rout.write('''<?xml version="1.0" encoding="UTF-8" standalone="no"?><templates>''')
rout.write('\n')
for fun in functions:
    if len(fun)<3 :continue#̫����
    if fun[0] in ('!','$','.','-','%','&','*','[','+','<','|',':','/','@','<','>') :continue#���ɵĺ�����������Щ��ͷ�����˵�
    if fun.count('<')>0:continue#�Ƿ��ַ���xml�н������ܳ���
    strFunc = "<template autoinsert=\"true\" context=\"r-code\" deleted=\"false\" description=\""+str(fun)+"   http://www.tanglei.name,You can modify this sentence by replacing them in the templetes.xml\" enabled=\"true\" name=\""+str(fun)+"\">"+str(fun)+"</template>\n"
    rout.write(strFunc)
    rfuns.write(fun)
    rfuns.write('\n')
rout.write('''</templates>''')
print('done')

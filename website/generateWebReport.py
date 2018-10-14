# coding= utf-8
import json

# python3
# generate web report from a stock list

# save to file: stock_web_report.html

# generate index
# 大盘，上证、深证、创业板、港美股

# DB_dict = {"000008" : {"code":"000008", "tag":"国资"}}

# python_to_json = json.dumps(test,ensure_ascii=False)

# 保证读写都是通过文件就不会有乱码的问题

with open('stockDB.json', 'r') as f:
	data = json.load(f)

# print(json.dumps(data, ensure_ascii=False))
# data.update(DB_dict)
print(DB_dict)

with open('stockDB.json', 'w') as f:
	json.dump(data, f, ensure_ascii=False, indent=4)
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
    <style>
        @page {
            margin: 0;
        }
        .p{
            text-indent: 2em;
            margin-bottom: 10px;

        }
        #title{
            margin-bottom: 10px;
            padding-bottom: 0px;
        }
        body{
            margin: 30px;
            padding: 0px;
            font: 100% SimSun, Microsoft YaHei, Times New Roman, Verdana, Arial, Helvetica, sans-serif;
            color: #000;
        }
        #div1{
            height: auto;
            width: auto;
            min-width: auto;
            margin: 0 auto;
            margin-top: 20px;
        }
        #div2{
            padding: 10px;
            padding-bottom: 0px;
        }
        #div3{
            list-style: none;
            margin-top: 22px;
            maigin-bottom: 10px;
            font-size: 14px;
        }
    </style>
</head>
<body>
<div id=div1>
    <div id=div2>
        <p id=title>尊敬的用户，您好：</p>
        <p class=p>您的验证码为：${code},15分钟内有效，请不要将验证码泄漏给其他人，如非本人请勿操作！</p>
        <div id=div3>
            <p class=p>金蜜工业云</p>
            <p class=p>${time}</p>
        </div>
    </div>
</div>
</body>
</html>

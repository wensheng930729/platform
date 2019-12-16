package com.bee.platform.user.utils;

import com.alibaba.fastjson.JSONObject;
import com.bee.platform.common.utils.ConstantsUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.StringUtils;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

/**
 * @Description 请求处理类
 * @author chenxm66777123
 * @Date 2019年03月20日
 * @version 1.0.0
 */
@Slf4j
public class RequestUtils {

    /**
     * @Description httpGet请求
     * @author chenxm66777123
     * @Date 2018年12月23日
     * @version 1.0.0
     */
    // 由于这类方法经常被用到,因此建议写在一个工具类里面,设置为静态方法,方便调用。
    // url表示请求链接,param表示json格式的请求参数
    public static JSONObject sendGetRequest(String url) {
        //调用贸易平台传入参数
        log.info("调用的贸易平台接口请求地址参数url：{}",url);
        String result = "";
        JSONObject jsonObject = null;
        try {
            //把字符串转换为URL请求地址
            URL requestRrl = new URL(url);
            // 打开连接
            HttpURLConnection connection = (HttpURLConnection) requestRrl.openConnection();
            // 连接会话
            connection.connect();
            // 获取输入流
            BufferedReader br = new BufferedReader(new InputStreamReader(connection.getInputStream(), "UTF-8"));
            String line;
            StringBuilder returnMsg = new StringBuilder();
            while ((line = br.readLine()) != null) {// 循环读取流
                returnMsg.append(line);
            }
            result = returnMsg.toString();
            log.info("调用贸易平台接口返回信息：{}",result);
            // 关闭流
            br.close();
            // 断开连接
            connection.disconnect();
            if (StringUtils.isEmpty(result)) {
                log.info("获取贸易接口信息失败：{}", result);
                //解析失败
                return null;
            }
            jsonObject = JSONObject.parseObject(result);
            String code = jsonObject.get("code").toString();
            if (!ConstantsUtil.FALSE.equals(code)) {
                log.info("调用贸易平台接口系统繁忙：{}", result);
                //解析失败
                return null;
            }
        } catch (Exception e) {
            log.error("调用http接口报错捕获失败处理日志:{}" + e.getMessage());
            e.printStackTrace();
            return null;
        }
        return  jsonObject;
    }

}

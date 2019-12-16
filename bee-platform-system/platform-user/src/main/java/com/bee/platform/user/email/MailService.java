package com.bee.platform.user.email;

import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.FreeMarkerType;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.DateUtils;
import com.sun.mail.util.MailSSLSocketFactory;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import javax.mail.*;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import java.security.GeneralSecurityException;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import static org.apache.commons.lang3.RandomStringUtils.randomNumeric;

/**
 * @ClassName MailService
 * @Description 邮件发送
 * @Author xin.huang
 * @Date 2019/4/25 16:48
 * @Version 1.0.0
 */
@Slf4j
@Component
public class MailService {

    @Autowired
    private JedisService jedisService;

    // SMTP服务器地址
    @Value("${spring.mail.host}")
    private String host;
    // 发送方邮箱
    @Value("${spring.mail.username}")
    private String from;
    // 发送方邮箱密码
    @Value("${spring.mail.password}")
    private String password;
    // 邮箱端口
    @Value("${spring.mail.port}")
    private String port;

    private static Properties prop = new Properties();

    private static Session session;

    private static MailSSLSocketFactory sf;

    @PostConstruct
    public void init(){
        prop.setProperty("mail.transport.protocol", "smtp");
        prop.setProperty("mail.smtp.host", host);
        prop.setProperty("mail.smtp.port", port);
        //使用smtp身份验证
        prop.setProperty("mail.smtp.auth", "true");

        //开启安全协议
        try {
            sf = new MailSSLSocketFactory();
        } catch (GeneralSecurityException e) {
            log.error("初始化MailSSLSocketFactory异常，异常信息是:{}",e);
            throw new BusinessException(ResCodeEnum.ERROR_SYSTEM, ExceptionMessageEnum.ERROR_SYSTEM);
        }
        sf.setTrustAllHosts(true);
        prop.put("mail.smtp.ssl.enable", "true");
        prop.put("mail.smtp.ssl.socketFactory", sf);
        //获取Session对象
        session = Session.getDefaultInstance(prop,new Authenticator() {
            @Override
            protected PasswordAuthentication getPasswordAuthentication() {
                return new PasswordAuthentication(from, password);
            }
        });
    }

    /**
     * @Description 修改密码发送邮件
     * @Param email 接收发邮箱
     * @return
     **/
    public ResponseResult<ResCodeEnum> sendMail(String email){
        //产生6位数的验证码
        String code = randomNumeric(6);
        Map<String, Object> map = new HashMap<>(1);
        map.put("code", code);
        map.put("time", DateUtils.format(new Date(),DateUtils.DEFAULT));
        try {
            // map中的key，对应模板中的${key}表达式
            String text = FreeMarkerFactory.getFreeMarkerFactory().render(FreeMarkerType.CODE_EMAIL,map);
            MimeMessage mimeMessage = new MimeMessage(session);
            mimeMessage.addHeader("X-Mailer","Microsoft Outlook Express 6.00.2900.2869");
            mimeMessage.setFrom(new InternetAddress(from,from));
            mimeMessage.addRecipient(Message.RecipientType.TO, new InternetAddress(email));
            //设置主题
            mimeMessage.setSubject(FreeMarkerType.CODE_EMAIL.getTitle());
            mimeMessage.setSentDate(new Date());
            //设置内容
            mimeMessage.setContent(text, "text/html;charset=utf-8");
            mimeMessage.saveChanges();
            //发送
            Transport.send(mimeMessage);
            log.info("**********************邮件发送成功************************");
            String key=ConstantsUtil.VALIDATE_CODE +email;
            jedisService.set(key,code,ConstantsUtil.SECOND);
            return ResponseResult.buildResponseResult(ResCodeEnum.SEND_MESSAGE_SUCCESS);
        } catch (Exception e) {
            log.error("邮箱验证码发送失败，邮箱是：{}，异常信息是：{}",email,e);
            return ResponseResult.buildResponseResult(ResCodeEnum.SEND_MESSAGE_FAIL);
        }
    }

    /**
     * @notes: 发生邮件
     * @Author: junyang.li
     * @Date: 16:06 2019/5/21
     * @param email : 目标邮件
     * @param type : 邮件模板类型
     * @param map : 邮件参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    public ResponseResult<ResCodeEnum> sendMail(String email,FreeMarkerType type,Map<String, Object> map){
        try {
            // map中的key，对应模板中的${key}表达式
            String text = FreeMarkerFactory.getFreeMarkerFactory().render(type,map);
            MimeMessage mimeMessage = new MimeMessage(session);
            mimeMessage.addHeader("X-Mailer","Microsoft Outlook Express 6.00.2900.2869");
            mimeMessage.setFrom(new InternetAddress(from,from));
            mimeMessage.addRecipient(Message.RecipientType.TO, new InternetAddress(email));
            //设置主题
            mimeMessage.setSubject(type.getTitle());
            mimeMessage.setSentDate(new Date());
            //设置内容
            mimeMessage.setContent(text, "text/html;charset=utf-8");
            mimeMessage.saveChanges();
            //发送
            Transport.send(mimeMessage);
            log.info("**********************邮件发送成功************************");
            return ResponseResult.buildResponseResult(ResCodeEnum.SEND_MESSAGE_SUCCESS);
        } catch (Exception e) {
            log.error("邮箱验证码发送失败，邮箱是：{}，异常信息是：{}",email,e);
            return ResponseResult.buildResponseResult(ResCodeEnum.SEND_MESSAGE_FAIL);
        }
    }


    /**
     * @notes: 邮箱验证码校验
     * @Author: junyang.li
     * @Date: 14:49 2019/5/10
     * @param email : 邮箱地址
     * @param code : 邮箱验证码
     * @return: boolean
     */
    public boolean checkVerificationCode(String email,String code){
        String key=ConstantsUtil.VALIDATE_CODE +email;
        //校验验证码
        String value=jedisService.get(key);
        if(value==null || code ==null){
            return false;
        }
        //验证码不正确
        boolean result= value.equals(code);
        //验证成功则删除缓存
        if(result){
            jedisService.delKey(key);
        }
        return result;
    }
}

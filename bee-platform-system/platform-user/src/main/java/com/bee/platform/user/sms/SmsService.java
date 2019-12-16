package com.bee.platform.user.sms;


import com.alibaba.fastjson.JSONObject;
import com.aliyuncs.DefaultAcsClient;
import com.aliyuncs.IAcsClient;
import com.aliyuncs.dysmsapi.model.v20170525.SendSmsRequest;
import com.aliyuncs.dysmsapi.model.v20170525.SendSmsResponse;
import com.aliyuncs.exceptions.ClientException;
import com.aliyuncs.http.MethodType;
import com.aliyuncs.profile.DefaultProfile;
import com.aliyuncs.profile.IClientProfile;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.ResourcesUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;

import static org.apache.commons.lang3.RandomStringUtils.randomNumeric;

@Slf4j
@Component
public class SmsService {
    @Autowired
    private JedisService jedisService;

    @Value("${aliyun.oss.accessKeyId}")
    private String accessKeyId;
    @Value("${aliyun.oss.accessKeySecret}")
    private String accessKeySecret;

    @Value("${aliyun.oss.smsRegister}")
    private String smsCode;

    @Value("${aliyun.oss.smsInvite}")
    private String smsInvite;

    @Value("${aliyun.oss.signName}")
    private String signName;
    /** 短信API产品名称（短信产品名固定，无需修改）*/
    private final String PRODUCT = "Dysmsapi";
    /** 短信API产品域名（接口地址固定，无需修改）*/
    private final String DOMAIN = "dysmsapi.aliyuncs.com";

    private IAcsClient iacsClient;

    private IAcsClient createClient() throws ClientException{
        if(iacsClient==null){
            log.info("****************************************初始化短信客户端，accessKeyId: "+accessKeyId+"*******************************************");
            //设置超时时间-可自行调整
            System.setProperty("sun.net.client.defaultConnectTimeout", "10000");
            System.setProperty("sun.net.client.defaultReadTimeout", "10000");
            IClientProfile profile = DefaultProfile.getProfile("cn-hangzhou", accessKeyId,
                    accessKeySecret);
            DefaultProfile.addEndpoint("cn-hangzhou", "cn-hangzhou", PRODUCT, DOMAIN);
            iacsClient= new DefaultAcsClient(profile);
        }
        return iacsClient;
    }
    /**
     * @notes 注册/修改密码发送短信
     * @Author junyang.li
     * @Date 11:43 2019/3/4
     **/
    public ResponseResult<ResCodeEnum> sendMessage(String phone) throws ClientException {
        IAcsClient acsClient =  createClient();
        //组装请求对象
        SendSmsRequest request = new SendSmsRequest();
        request.setMethod(MethodType.POST);
        //必填:待发送手机号
        request.setPhoneNumbers(phone);
        //必填:短信签名-可在短信控制台中找到
        request.setSignName(signName);
        //必填:短信模板-可在短信控制台中找到
        request.setTemplateCode(smsCode);
        String code = randomNumeric(6);
        Map<String,Object> map=new HashMap<>(1);
        map.put("code",code);
        //"{\"code\": \"" + code + "\"}"
        request.setTemplateParam(JSONObject.toJSONString(map));
        SendSmsResponse sendSmsResponse = acsClient.getAcsResponse(request);
        if(ConstantsUtil.OK.equals(sendSmsResponse.getCode())){
            log.info("**********************短信发送成功************************");
            jedisService.set(ConstantsUtil.VALIDATE_CODE+phone,code,ConstantsUtil.SECOND);
            return ResponseResult.buildResponseResult(ResCodeEnum.SEND_MESSAGE_SUCCESS);
        }
        log.error("用户注册短信认证码发送失败，errorCode={},errorMessage={}",sendSmsResponse.getCode(),sendSmsResponse.getMessage());
        return ResponseResult.buildResponseResult(ResCodeEnum.SEND_MESSAGE_FAIL);
    }

    /**
     * @notes 发送邀请短信
     * @Author junyang.li
     * @Date 11:43 2019/3/4
     **/
    public ResponseResult<ResCodeEnum> sendMessageForInvit(String phone ,String org_name) throws ClientException{
        IAcsClient acsClient = createClient();
        //组装请求对象
        SendSmsRequest request = new SendSmsRequest();
        request.setMethod(MethodType.POST);
        //必填:待发送手机号
        request.setPhoneNumbers(phone);
        //必填:短信签名-可在短信控制台中找到
        request.setSignName(signName);
        //必填:短信模板-可在短信控制台中找到
        request.setTemplateCode(smsInvite);
        request.setTemplateParam("{\"org_name\": \"" + org_name + "\"}");
        SendSmsResponse sendSmsResponse = acsClient.getAcsResponse(request);
        if(ConstantsUtil.OK.equals(sendSmsResponse.getCode())){
            log.info("**********************短信发送成功************************");
            return ResponseResult.buildResponseResult(ResCodeEnum.SEND_MESSAGE_SUCCESS);
        }
        log.error("用户邀请短信发送失败，errorCode={},errorMessage={}",sendSmsResponse.getCode(),sendSmsResponse.getMessage());
        return ResponseResult.buildResponseResult(ResCodeEnum.SEND_MESSAGE_FAIL);
    }

    /**
     * @notes: 发送提示类短信
     * @Author: junyang.li
     * @Date: 15:56 2019/5/10
     * @param phone : 手机号
     * @param key : 短信模板key对应的配置文件中的key
     * @param map : 提示消息map对象
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Async
    public ResponseResult<ResCodeEnum> sendMessageForPrompt(String phone , Integer key,Map<String,Object> map) throws ClientException{
        return sendMessageForPrompt(phone,key,JSONObject.toJSONString(map));
    }

    /**
     * @Description 发送提示类短信
     * @Param phone
     * @Param key 短信模板key对应的配置文件中的key
     * @Param notice 提示消息
     * @Author xin.huang
     * @Date 17:10 2019/5/7
     */
    @Async
    public ResponseResult<ResCodeEnum> sendMessageForPrompt(String phone , Integer key, String notice) throws ClientException{
        IAcsClient acsClient = createClient();
        //组装请求对象
        SendSmsRequest request = new SendSmsRequest();
        request.setMethod(MethodType.POST);
        //必填:待发送手机号
        request.setPhoneNumbers(phone);
        //必填:短信签名-可在短信控制台中找到
        request.setSignName(signName);
        //必填:短信模板-可在短信控制台中找到
        String invite = ResourcesUtil.getValue(key);
        if (StringUtils.isBlank(invite)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SEND_MESSAGE_FAIL);
        }
        request.setTemplateCode(invite);
        request.setTemplateParam(notice);
        SendSmsResponse sendSmsResponse = acsClient.getAcsResponse(request);
        if(ConstantsUtil.OK.equals(sendSmsResponse.getCode())){
            log.info("**********************短信发送成功************************");
            return ResponseResult.buildResponseResult(ResCodeEnum.SEND_MESSAGE_SUCCESS);
        }
        log.error("用户操作提示短信发送失败，errorCode={},errorMessage={}",sendSmsResponse.getCode(),sendSmsResponse.getMessage());
        return ResponseResult.buildResponseResult(ResCodeEnum.SEND_MESSAGE_FAIL);
    }


    /**
     * @notes: 手机证码校验
     * @Author: junyang.li
     * @Date: 14:49 2019/5/10
     * @param phone : 手机账号
     * @param code : 手机验证码
     * @return: boolean
     */
    @SuppressWarnings("all")
    public boolean checkVerificationCode(String phone,String code){
        String key=ConstantsUtil.VALIDATE_CODE+phone;
        //校验验证码
        String value=jedisService.get(key);
        if(value==null || code ==null){
            return false;
        }
        //验证码不正确
        boolean result= value.equals(code);
        //验证成功则删除缓存
        if(!result){
            log.info("验证不通过,验证码不正确。输入的code={}，缓存中的code={}", code, value);
            return result;
        }
        jedisService.delKey(key);
        //并且缓存校验结果
        jedisService.set(ConstantsUtil.VALIDATE_RESULT + phone,
                ConstantsUtil.TRUE, ConstantsUtil.FIFTEEN_MINUTE);
        return result;
    }

    /**
     * @notes: 获得缓存的校验结果
     * @Author: junyang.li
     * @Date: 14:46 2019/5/15
     * @param phone : 待检验账号
     * @return: boolean
     */
    public boolean getCheckResult(String phone){
        String key=ConstantsUtil.VALIDATE_RESULT + phone;
        log.info("校验验证码,缓存的键是:{}",key);
        String result=jedisService.get(key);
        log.info("从缓存获得的返回结果是:{}",result);
        if(result==null || Status.FALSE.getKey().equals(Integer.valueOf(result))){
            return false;
        }
        //验证成功则删除缓存
        jedisService.delKey(key);
        return true;
    }

}

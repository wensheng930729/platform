package com.bee.platform.user.geetest.check;

import com.bee.bcrypt.AESUtil;
import com.bee.platform.common.enums.GeetestClientType;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.config.GeetestConfig;
import com.bee.platform.user.geetest.sdk.GeetestLib;
import com.bee.platform.user.vo.ManagerLoginVO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashMap;

/**
 * @notes 使用post方式，返回验证结果, request表单中必须包含challenge, validate, seccode
 *  登录校验
 * @Author junyang.li
 * @Date 9:00 2019/4/29
 **/
@Slf4j
@Component
public class VerifyLogin {

	@Autowired
	private GeetestConfig geetestConfig;

	@Autowired
	private JedisService jedisService;

	public boolean verify(ManagerLoginVO vo){
		GeetestLib gtSdk = new GeetestLib(geetestConfig.getGeetestId(), geetestConfig.getGeetestKey(),
				geetestConfig.getNewfailBack());
			
		String challenge =null;// vo.getChallenge();
		System.out.println("-------challenge="+challenge);
		String validate =null;// vo.getValidate();
		System.out.println("-------validate="+validate);
		String seccode =null;// vo.getSeccode();
		System.out.println("-------seccode="+seccode);
        String first=ConstantsUtil.GEETEST_FRSIT_VERIFY;
        String result=jedisService.get(first);
        if(result==null){
            return false;
        }
		//从session中获取gt-server状态
		int status = Integer.valueOf(result);
		
		//从session中获取userid
        String userId= AESUtil.encrypt(vo.getPassword(),vo.getUsername());

		//自定义参数,可选择添加
		HashMap<String, String> param = new HashMap<String, String>(16);
        //网站用户id
		param.put("user_id", userId);
        //web:电脑上的浏览器；h5:手机上的浏览器，包括移动应用内完全内置的web_view；native：通过原生SDK植入APP应用的方式
		param.put("client_type", GeetestClientType.WEB.getDesc());
        //传输用户请求验证时所携带的IP
		param.put("ip_address", "127.0.0.1");
		int gtResult;
		if (status == 1) {
			//gt-server正常，向gt-server进行二次验证
			gtResult = gtSdk.enhencedValidateRequest(challenge, validate, seccode, param);

		} else {
			// gt-server非正常情况下，进行failback模式验证
			log.error("failback:use your own server captcha validate");
			gtResult = gtSdk.failbackValidateRequest(challenge, validate, seccode);
		}
		log.info("极验校验结果是：{}",gtResult);
        //清除第一次校验的结果
        jedisService.delKey(first);
		return gtResult == 1;
	}
}

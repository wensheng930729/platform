package com.bee.platform.user.geetest.check;

import com.alibaba.fastjson.JSONObject;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.GeetestClientType;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.config.GeetestConfig;
import com.bee.platform.user.dto.GeetestDTO;
import com.bee.platform.user.geetest.sdk.GeetestLib;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashMap;



/**
 * 使用Get的方式返回challenge和capthca_id,此方式以实现前后端完全分离的开发模式
 *
 */

@Component
public class StartCaptcha {

	@Autowired
	private GeetestConfig geetestConfig;

	@Autowired
	private JedisService jedisService;

	public ResponseResult<GeetestDTO> initialize() {

		GeetestLib gtSdk = new GeetestLib(geetestConfig.getGeetestId(), geetestConfig.getGeetestKey(),
				geetestConfig.getNewfailBack());
		//用户账户加密
		//String userId= AESUtil.encrypt(username,username);
		//自定义参数,可选择添加
		HashMap<String, String> param = new HashMap<String, String>(16);
		//param.put("user_id", userId); //网站用户id
		param.put("client_type", GeetestClientType.WEB.getDesc()); //web:电脑上的浏览器；h5:手机上的浏览器，包括移动应用内完全内置的web_view；native：通过原生SDK植入APP应用的方式
		param.put("ip_address", "127.0.0.1"); //传输用户请求验证时所携带的IP
		//进行验证预处理
		int gtServerStatus = gtSdk.preProcess(param);
        String resStr = gtSdk.getResponseStr();
        GeetestDTO dto=JSONObject.parseObject(resStr,GeetestDTO.class).setClientType(GeetestClientType.WEB.getDesc());
		String first=ConstantsUtil.GEETEST_FRSIT_VERIFY;
		jedisService.set(first, String.valueOf(gtServerStatus), ConstantsUtil.OVERDUE);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
	}
}

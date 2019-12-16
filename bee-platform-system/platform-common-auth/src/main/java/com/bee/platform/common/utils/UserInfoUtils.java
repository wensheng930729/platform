package com.bee.platform.common.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import com.bee.platform.common.entity.Page;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

import com.alibaba.fastjson.JSONObject;
import com.bee.platform.common.config.property.CommonAuthConfigProperties;
import com.bee.platform.common.dto.AuthInterfaceRQ;
import com.bee.platform.common.dto.AuthResourceDetailDTO;
import com.bee.platform.common.dto.UserAuthValidateDTO;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.google.common.collect.Maps;

import lombok.extern.slf4j.Slf4j;

/**
 * @author liang.li
 * @ClassName UserInfoUtils
 * @Description 获取当前登录用户信息
 * @Date 2018-12-27 14:29
 */
@Slf4j
@Component("authUserInfoUtils")
public class UserInfoUtils {
	
    /**
     * 是否启用全局权限验证的开关
     */
    public static final String ENABLE_AUTHCHECK_SWICH = "enable_authcheck_swich";
    
    private static final String SIMPLE_USERINFO_PATH = "authPlatformUser/simpleUserInfo";
    private static final String USER_ROLEINFO_PATH = "authPlatformUser/userRoleInfo";
    private static final String VALIDATE_PRIVILEGE_PATH = "authPlatformUser/validate/privilege";
    private static final String MENU_PATH = "authResource/resourcesByUser";
    private static final String SYSTEMCONFIG_PATH = "authPlatformConf/getSystemConf";
    private static final String ADDBYROUTERURLMETHOD_PATH = "authInterface/addByRouterUrlMethod";
    private static final String USER_RESOURCE_BY_ROLEID="/authResource/getUserResourceByRoleId";
    private static final String GET_USER_BY_KEYWORD="/authPlatformUser";
    /**
     * 根据路由、URL、请求方式自动获取接口资源的开关
     */
    public static final String AUTOGETRESOURCE_SWICH = "autogetresource_swich";

    @Autowired
    private RestTemplate restTemplate;
    
    @Autowired
    CommonAuthConfigProperties props;
    
    /**
     * 根据路由、URL、请求方式自动获取接口信息并保存到auth_interface表中，如果该接口已经存在则跳过
     * @param authInterfaceRQ
     */
	public ResponseResult<ResCodeEnum> addByRouterUrlMethod(AuthInterfaceRQ authInterfaceRQ) {
		HttpHeaders headers = setInnerCallHeader(MediaType.APPLICATION_JSON);
		HttpEntity<AuthInterfaceRQ> requestEntity = new HttpEntity<>(authInterfaceRQ, headers);
		ResponseEntity<ResponseResult<ResCodeEnum>> resp = restTemplate.exchange(
				props.getExpectedAuthAddress() + ADDBYROUTERURLMETHOD_PATH, HttpMethod.POST, requestEntity,
				new ParameterizedTypeReference<ResponseResult<ResCodeEnum>>() {
				});
		// 处理响应结果
		if (null != resp && HttpStatus.OK == HttpStatus.valueOf(resp.getStatusCodeValue())) {
			return resp.getBody();
		}
		return null;
	}
    
	/**
	 * 根据路由、URL、请求方式自动获取接口资源的开关
	 * @param key 配置的键
	 * @param value 配置的值
	 * @param desc 配置的描述信息
	 */
	public String getSystemConf(String key, String value, String desc) {
		// 请求头信息
		HttpHeaders headers = setInnerCallHeader(MediaType.APPLICATION_FORM_URLENCODED);
		// 封装参数，千万不要替换为Map与HashMap，否则参数无法传递
		MultiValueMap<String, String> params = new LinkedMultiValueMap<String, String>();
		params.add("confKey", key);
		params.add("confValue", value);
		params.add("confDesc", desc);
		HttpEntity<MultiValueMap<String, String>> requestEntity = new HttpEntity<MultiValueMap<String, String>>(params,
				headers);
		// 发送POST请求
		ResponseEntity<ResponseResult<String>> resp = restTemplate.exchange(
				props.getExpectedAuthAddress() + SYSTEMCONFIG_PATH, HttpMethod.POST, requestEntity,
				new ParameterizedTypeReference<ResponseResult<String>>() {
				});
		// 处理响应结果
		if (null != resp && HttpStatus.OK == HttpStatus.valueOf(resp.getStatusCodeValue())) {
			ResponseResult<String> responseResult = resp.getBody();
			if (null != responseResult && ResCodeEnum.SUCCESS.code.equals(responseResult.getCode()) ) {
				return responseResult.getObject();
			}
		}
		return null;
	}

    /******************公共权限新接口*******************/
    /**
     * 获取用户基础信息,不包括角色
     * @param request
     * @return
     */
    public AuthPlatformUserInfo getSimpleUserInfo(HttpServletRequest request) {
        // 获取当前用户信息
        String sysToken = AuthWebUtils.getParam(AuthConstantsUtil.SYS_TOKEN,request);
        return getSimpleUserInfoByToken(sysToken);
    }

    /**
     * 通过sysToken 获取用户基础信息,不包括角色
     * @param sysToken
     * @return
     */
    public AuthPlatformUserInfo getSimpleUserInfoByToken(String sysToken){
        if(StringUtils.isBlank(sysToken)){
            log.error("未获取到sys_token");
            return null;
        }
        HttpHeaders headers = setInnerCallHeader(MediaType.APPLICATION_FORM_URLENCODED);
        HttpEntity<Object> requestEntity = new HttpEntity<>(headers);
        ResponseEntity<ResponseResult<AuthPlatformUserInfo>> resp = restTemplate.exchange(props.getExpectedAuthAddress() + SIMPLE_USERINFO_PATH + "?sysToken={sysToken}", HttpMethod.GET, requestEntity, new ParameterizedTypeReference<ResponseResult<AuthPlatformUserInfo>>(){},sysToken);
        if (null != resp && HttpStatus.OK == HttpStatus.valueOf(resp.getStatusCodeValue())) {
        	ResponseResult<AuthPlatformUserInfo> responseResult = resp.getBody();
        	if(null != responseResult) {
        		  AuthPlatformUserInfo userInfo = responseResult.getObject();
                  return userInfo;
        	}
        }
		return null;
    }
    /**
     * 获取用户信息包括权限信息
     * @param userName 用户名即手机号码
     * @return
     */
    public AuthPlatformUserInfo getUserRoleInfo(String userName) {
        return getUserRoleInfoByToken(null,userName,null);
    }


    /**
     * 获取用户信息包括权限信息
     * @param sysToken 用户平台token
     * @return
     */
    public AuthPlatformUserInfo getUserRoleInfoByToken(String sysToken) {
        return getUserRoleInfoByToken(sysToken,null,null);
    }

    /**
     * 获取用户信息包括权限信息
     * @param sysToken 用户平台token
     * @return
     */
    public AuthPlatformUserInfo getUserRoleInfoByToken(String sysToken, String sysType) {
        return getUserRoleInfoByToken(sysToken,null,sysType);
    }

    /**
     * 获取用户信息包括权限信息
     * @param sysToken 用户平台token
     * @return
     */
    public AuthPlatformUserInfo getUserRoleInfoByToken(String sysToken,String username ,String sysType) {
        StringBuilder builder=new StringBuilder(props.getExpectedAuthAddress()).append(USER_ROLEINFO_PATH);
        String symbol="?";
        if(StringUtils.isNotEmpty(sysToken)){
            builder.append(symbol).append("sysToken=").append(sysToken);
            symbol="&";
        }
        if(StringUtils.isNotEmpty(username)){
            builder.append(symbol).append("username=").append(username);
            symbol="&";
        }
        if (StringUtils.isNotEmpty(sysType)) {
            builder.append(symbol).append("sysType=").append(sysType);
        }
        return getUserRoleInfoByUrl(builder.toString());
    }

    /**
     * 获取用户信息包括权限信息
     * @param url 用户平台token
     * @return
     */
    private AuthPlatformUserInfo getUserRoleInfoByUrl(String url) {
        // 获取当前用户信息
        HttpHeaders headers = setInnerCallHeader(MediaType.APPLICATION_FORM_URLENCODED);
        HttpEntity<Object> requestEntity = new HttpEntity<>(headers);
        ResponseEntity<ResponseResult<AuthPlatformUserInfo>> resp = restTemplate.exchange(url, HttpMethod.GET , requestEntity, new ParameterizedTypeReference<ResponseResult<AuthPlatformUserInfo>>(){});
        if (null != resp && HttpStatus.OK == HttpStatus.valueOf(resp.getStatusCodeValue())) {
        	ResponseResult<AuthPlatformUserInfo> responseResult = resp.getBody();
        	if(null != responseResult) {
        		AuthPlatformUserInfo userInfo = responseResult.getObject();
                log.info("从平台查询角色返回的message是：{}。请求的url是{}", null != responseResult ? JSONObject.toJSONString(responseResult.getMessage()) : null,url);
                return userInfo;
        	}
        }
		return null;
    }
    
    /**
     * 权限验证
     * @param rq
     * @return
     */
    public boolean validatePrivilege(UserAuthValidateDTO rq) {
        HttpHeaders headers = setInnerCallHeader(MediaType.APPLICATION_JSON);
        HttpEntity<UserAuthValidateDTO> requestEntity = new HttpEntity<>(rq, headers);
        ResponseEntity<ResponseResult<Boolean>> resp = restTemplate.exchange(props.getExpectedAuthAddress() + VALIDATE_PRIVILEGE_PATH, HttpMethod.POST, requestEntity, new ParameterizedTypeReference<ResponseResult<Boolean>>(){});
        if (null != resp && HttpStatus.OK == HttpStatus.valueOf(resp.getStatusCodeValue())) {
        	ResponseResult<Boolean> responseResult = resp.getBody();
        	if(null != responseResult ) {
        		if (ResCodeEnum.SUCCESS.getCode().equals(responseResult.getCode())) {
                    return responseResult.getObject().booleanValue();
                } else {
                    log.error("权限校验出错, {}", responseResult.getMessage());
                    return false;
                }
        	}
        }
		return false;
    }
    
    /**
     * 根据用户名userName获取用户菜单
     * @param sysToken
     * @return
     */
    public List<AuthResourceDetailDTO> getMenus(String sysToken) {
        HttpHeaders headers = setInnerCallHeader(MediaType.APPLICATION_FORM_URLENCODED);
        Map<String, Object> paramMap=Maps.newHashMap();  
        paramMap.put("subSys", props.getPlatform());
        paramMap.put("sysToken", sysToken);
        HttpEntity<Object> requestEntity = new HttpEntity<>(headers);
        ResponseEntity<ResponseResult<List<AuthResourceDetailDTO>>> resp = restTemplate.exchange(props.getExpectedAuthAddress() + MENU_PATH + "?subSys={subSys}&sysToken={sysToken}", HttpMethod.GET, requestEntity, new ParameterizedTypeReference<ResponseResult<List<AuthResourceDetailDTO>>>(){}, paramMap);
        return resultData(resp);
    }

    public List<AuthResourceDetailDTO> getMenus(String sysToken,Integer roleId) {
        HttpHeaders headers = setInnerCallHeader(MediaType.APPLICATION_FORM_URLENCODED);
        Map<String, Object> paramMap=Maps.newHashMap();
        paramMap.put("subSys", props.getPlatform());
        paramMap.put("sysToken", sysToken);
        paramMap.put("roleId", roleId);
        HttpEntity<Object> requestEntity = new HttpEntity<>(headers);
        ResponseEntity<ResponseResult<List<AuthResourceDetailDTO>>> resp = restTemplate.exchange(props.getExpectedAuthAddress() +
                USER_RESOURCE_BY_ROLEID + "?sysToken={sysToken}&subSys={subSys}&roleId={roleId}", HttpMethod.GET, requestEntity,
                new ParameterizedTypeReference<ResponseResult<List<AuthResourceDetailDTO>>>(){}, paramMap);
        return resultData(resp);
    }


    private List<AuthResourceDetailDTO> resultData(ResponseEntity<ResponseResult<List<AuthResourceDetailDTO>>> resp ){
        if (null != resp && HttpStatus.OK == HttpStatus.valueOf(resp.getStatusCodeValue())) {
            ResponseResult<List<AuthResourceDetailDTO>> responseResult = resp.getBody();
            if(null != responseResult) {
                if (null != resp && ResCodeEnum.SUCCESS.getCode().equals(responseResult.getCode())) {
                    return responseResult.getObject();
                } else {
                    log.error("获取用户菜单出错, {}", responseResult.getMessage());
                    return null;
                }
            }
        }
        return null;
    }

    /**
     * 设置内部调用头信息
     * @param mt
     * @return
     */
    private HttpHeaders setInnerCallHeader(MediaType mt) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(mt);
        headers.add(AuthConstantsUtil.INNER_CLIENT_ID, AuthConstantsUtil.PLATFORM);
        return headers;
    }

    /**
     * @notes: 通过关键词查询用户信息
     * @Author: junyang.li
     * @Date: 13:39 2019/6/19
     * @param sysToken :
     * @param keyWord :
     * @return: java.util.List<com.bee.platform.common.entity.AuthPlatformUserInfo>
     */
    public String getUserByKeyWord(String sysToken, String keyWord,Page page){
        // 请求头信息
        HttpHeaders headers = setInnerCallHeader(MediaType.APPLICATION_FORM_URLENCODED);
        // 封装参数，千万不要替换为Map与HashMap，否则参数无法传递
        MultiValueMap<String, String> params = new LinkedMultiValueMap<String, String>();
        params.add("sysToken", sysToken);
        params.add("keyWord", keyWord);
        params.add("page", JSONObject.toJSONString(page));
        HttpEntity<MultiValueMap<String, String>> requestEntity = new HttpEntity<MultiValueMap<String, String>>(params,
                headers);
        // 发送POST请求
        ResponseEntity<ResponseResult<List<AuthPlatformUserInfo>>> resp = restTemplate.exchange(
                props.getExpectedAuthAddress() + GET_USER_BY_KEYWORD, HttpMethod.POST, requestEntity,
                new ParameterizedTypeReference<ResponseResult<List<AuthPlatformUserInfo>>>() {
                });
        // 处理响应结果
        if (HttpStatus.OK == HttpStatus.valueOf(resp.getStatusCodeValue())) {
            return JSONObject.toJSONString(resp.getBody());
        }
        return JSONObject.toJSONString(ResponseResult.buildResponseResult(ResCodeEnum.ERROR_SYSTEM));
    }

}

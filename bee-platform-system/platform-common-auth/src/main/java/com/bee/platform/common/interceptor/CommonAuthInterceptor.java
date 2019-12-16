package com.bee.platform.common.interceptor;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.servlet.HandlerInterceptor;

import com.bee.platform.common.config.property.CommonAuthConfigProperties;
import com.bee.platform.common.dto.AuthInterfaceRQ;
import com.bee.platform.common.dto.UserAuthValidateDTO;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.exception.CommonAuthorityException;
import com.bee.platform.common.exception.CommonAuthorityForbiddenException;
import com.bee.platform.common.utils.AuthConstantsUtil;
import com.bee.platform.common.utils.AuthWebUtils;
import com.bee.platform.common.utils.UserInfoUtils;
import com.google.common.base.Joiner;

import lombok.extern.slf4j.Slf4j;

/**
 * @author Raphael.dq
 * @date 2019/05/21
 */
@Slf4j
public class CommonAuthInterceptor implements HandlerInterceptor {
    
    @Autowired
    CommonAuthConfigProperties properties;
    
    @Autowired
    RestTemplate template;
    
    @Autowired
    UserInfoUtils userUtil;

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler)
        throws Exception {
        // 服务内部调用,放行
        String innerFlag = request.getHeader(AuthConstantsUtil.INNER_CLIENT_ID);
        if (StringUtils.isNotEmpty(innerFlag)) {
            return true;
        }
        String beeRouter = request.getHeader(AuthConstantsUtil.BEE_ROUTER);
        String method = request.getMethod();
        String srcUri = request.getRequestURI();
        String uri = srcUri.replaceFirst("/+", "/");
        String matchUri = Joiner.on("_").skipNulls().join(method, uri);
        log.info("Intercept request uri: {}", matchUri);
        // 获取自动插入路由、URL、请求方式接口资源的开关
        String autogetresourceSwich = userUtil.getSystemConf(UserInfoUtils.AUTOGETRESOURCE_SWICH, "1", "根据路由、URL、请求方式自动获取接口资源的开关");
        if("1".equals(autogetresourceSwich) && StringUtils.isBlank(innerFlag) 
        		&& StringUtils.isNotBlank(beeRouter) 
        		&& StringUtils.isNotBlank(method) 
        		&& StringUtils.isNotBlank(uri)) {
        	AuthInterfaceRQ authInterfaceRQ = new AuthInterfaceRQ().setBeeRouter(beeRouter)
																	.setType(method)
																	.setUrl(uri)
																	.setOrderNum(1)
																	.setName("待确认的接口")
																	.setSubSys("待确认的子系统");
        	// 调用公共权限管理的接口添加资源
        	ResponseResult<ResCodeEnum> responseResult = userUtil.addByRouterUrlMethod(authInterfaceRQ);
			log.info("自动插入路由、URL、请求方式接口资源,code={}, msg={}, authInterfaceRQ={}", responseResult.getCode(),
					responseResult.getMessage(), authInterfaceRQ);
        }
		// 获取全局权限校验的开关
		String enableAuthcheckSwich = userUtil.getSystemConf(UserInfoUtils.ENABLE_AUTHCHECK_SWICH, "1",
				"是否启用全局权限校验的开关:1启用,0禁用");
		log.info("全局权限校验的开关1启用,0禁用,enableAuthcheckSwich={}", enableAuthcheckSwich);
		if (!"1".equals(enableAuthcheckSwich)) {
			return true;
		}
        // 校验资源
        if (validate(request, uri, method)) {
            return true;
        } else {
            throw new CommonAuthorityForbiddenException("权限验证失败,用户无权访问");
        }
        
    }

    /**
     * 根据用户不同角色类型校验用户是否有权访问该matchUri
     * @param request
     * @param matchUri
     * @return
     */
    private boolean validate(HttpServletRequest request, String uri, String method) {
        String sysToken = AuthWebUtils.getParam(AuthConstantsUtil.SYS_TOKEN,request);
        if (sysToken == null) {
            throw new CommonAuthorityException("未获取到用户sys_token信息");
        }
        UserAuthValidateDTO rq = new UserAuthValidateDTO().setUri(uri).setMethod(method).setToken(sysToken);
        return userUtil.validatePrivilege(rq);
        
    }

}

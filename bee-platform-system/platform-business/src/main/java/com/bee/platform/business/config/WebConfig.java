 package com.bee.platform.business.config;

 import com.bee.platform.common.entity.AuthPlatformUserInfo;
 import com.bee.platform.common.entity.UserInfo;
 import com.bee.platform.common.utils.UserInfoUtils;
 import lombok.extern.slf4j.Slf4j;
 import org.springframework.context.annotation.Configuration;
 import org.springframework.core.MethodParameter;
 import org.springframework.web.bind.support.WebDataBinderFactory;
 import org.springframework.web.context.request.NativeWebRequest;
 import org.springframework.web.method.support.HandlerMethodArgumentResolver;
 import org.springframework.web.method.support.ModelAndViewContainer;
 import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
 import org.springframework.web.servlet.config.annotation.WebMvcConfigurationSupport;

 import javax.servlet.http.HttpServletRequest;
 import java.util.List;

/**
 * @author Raphael.dq
 * @date 2019/03/04
 */
@Slf4j
 @Configuration
public class WebConfig extends WebMvcConfigurationSupport{

    @Override
    protected void addArgumentResolvers(List<HandlerMethodArgumentResolver> argumentResolvers) {
        argumentResolvers.add(new UserArgumentResolver());
        super.addArgumentResolvers(argumentResolvers);
    }

    @Override
    protected void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler("swagger-ui.html")
                .addResourceLocations("classpath:/META-INF/resources/");
        registry.addResourceHandler("/webjars/**")
                .addResourceLocations("classpath:/META-INF/resources/webjars/");
    }
    

    static class UserArgumentResolver implements HandlerMethodArgumentResolver {

        @Override
        public boolean supportsParameter(MethodParameter parameter) {
             return UserInfo.class.isAssignableFrom(parameter.getParameterType());
        }

        @Override
        public Object resolveArgument(MethodParameter parameter, ModelAndViewContainer mavContainer,
            NativeWebRequest webRequest, WebDataBinderFactory binderFactory) throws Exception {
            HttpServletRequest servletRequest = webRequest.getNativeRequest(HttpServletRequest.class);
            try {
                //UserInfo user = UserInfoUtils.getUserInfo(servletRequest);
                AuthPlatformUserInfo userInfo = new UserInfoUtils().getSimpleUserInfo(servletRequest);
                return userInfo;
            } catch (Exception e) {
                log.warn("UserInfo参数解析错误",  e);
                throw new IllegalArgumentException("获取用户信息失败", e);
            }
        }
        
    }

}

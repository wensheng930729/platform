package com.bee.platform.user.hystrix;

import com.bee.platform.common.dto.PlatformManagersDTO;
import com.bee.platform.common.entity.*;
import com.bee.platform.user.authority.dto.AuthResourceDetailDTO;
import com.bee.platform.user.dto.AuthPlatformUserEnterpriseDTO;
import com.bee.platform.user.service.feign.AuthResourcesFeignClient;
import com.bee.platform.user.service.feign.UserInfoFeignClient;
import feign.hystrix.FallbackFactory;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

@Component
public class AuthResourcesFeignClientFallbackFactory implements FallbackFactory<AuthResourcesFeignClient> {

	@Override
	public AuthResourcesFeignClient create(Throwable cause) {

		return new AuthResourcesFeignClient() {

			@Override
			public ResponseResult<List<AuthResourceDetailDTO>> listResourcesByUser(@RequestParam String subSys,
																				   @RequestParam(required = false) String sysToken) {
				return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED,null);
			}
		};
	}

}

package com.bee.platform.user.hystrix;

import com.bee.platform.common.entity.EnterprisesUsers;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.dto.EnterprisesUsersInfoDTO;
import com.bee.platform.user.service.feign.EnterprisesUsersFeiginClient;
import feign.hystrix.FallbackFactory;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.RequestParam;

@Component
public class EnterprisesUsersFeiginClientFallbackFactory implements FallbackFactory<EnterprisesUsersFeiginClient>{

	@Override
	public EnterprisesUsersFeiginClient create(Throwable cause) {
		return new EnterprisesUsersFeiginClient() {

			@Override
			public ResponseResult<EnterprisesUsers> add(@RequestParam("sysToken") String sysToken, EnterprisesUsersInfoDTO enterprisesUsersInfoDTO) {
				return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, ResCodeEnum.HYSTRIX_ENABLED.msg);
			}
		};
	}
}

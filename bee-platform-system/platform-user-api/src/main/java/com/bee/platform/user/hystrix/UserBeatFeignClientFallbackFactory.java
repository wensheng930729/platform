package com.bee.platform.user.hystrix;

import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.service.feign.UserBeatFeignClient;
import feign.hystrix.FallbackFactory;
import org.springframework.stereotype.Component;

@Component
public class UserBeatFeignClientFallbackFactory implements FallbackFactory<UserBeatFeignClient> {

	@Override
	public UserBeatFeignClient create(Throwable cause) {

		return new UserBeatFeignClient() {

			@Override
			public ResponseResult<String> beat() {
				return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, ResCodeEnum.HYSTRIX_ENABLED.msg);
			}

		};
	}

}

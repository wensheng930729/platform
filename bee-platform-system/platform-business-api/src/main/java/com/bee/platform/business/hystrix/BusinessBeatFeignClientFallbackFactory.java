package com.bee.platform.business.hystrix;

import com.bee.platform.business.service.feign.BusinessBeatFeignClient;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import feign.hystrix.FallbackFactory;
import org.springframework.stereotype.Component;

@Component
public class BusinessBeatFeignClientFallbackFactory implements FallbackFactory<BusinessBeatFeignClient> {

	@Override
	public BusinessBeatFeignClient create(Throwable cause) {

		return new BusinessBeatFeignClient() {

			@Override
			public ResponseResult<String> beat() {
				return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED, ResCodeEnum.HYSTRIX_ENABLED.msg);
			}

		};
	}

}

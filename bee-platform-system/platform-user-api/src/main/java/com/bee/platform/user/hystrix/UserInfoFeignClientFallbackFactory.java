package com.bee.platform.user.hystrix;

import com.bee.platform.common.entity.*;
import com.bee.platform.common.dto.PlatformManagersDTO;
import com.bee.platform.user.authority.dto.AuthPlatformUserPullDownDto;
import com.bee.platform.user.dto.AuthPlatformUserEnterpriseDTO;
import com.bee.platform.user.service.feign.UserInfoFeignClient;
import feign.hystrix.FallbackFactory;

import java.util.List;

import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.RequestHeader;

@Component
public class UserInfoFeignClientFallbackFactory implements FallbackFactory<UserInfoFeignClient> {

	@Override
	public UserInfoFeignClient create(Throwable cause) {

		return new UserInfoFeignClient() {

			@Override
			public ResponseResult<UserInfo> getUserInfo(String sysToken) {
				return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED,null);
			}

			@Override
			public ResponseResult<EnterprisesUsers> getEnterpriseUserInfoById(Integer userId, Integer enterpriseId) {
				return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED,null);
			}

			@Override
			public ResponseResult<PlatformManagersDTO> getManagerByName(String userName) {
				return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED,null);
			}

			@Override
			public ResponseResult<ResCodeEnum> resetUserInvited() {
				return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED);
			}

			@Override
			public List<AuthPlatformUserEnterpriseDTO> qureyEnterpriseUser(int userId) {
				return null;
			}

			@Override
			public ResponseResult<ManagerInfo> getManagerInfo(String sysToken) {
				return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED,null);
			}

			@Override
			public ResponseResult<List<AuthPlatformUserPullDownDto>> getAllCompanyUserById(Integer orgId){
				return ResponseResult.buildResponseResult(ResCodeEnum.HYSTRIX_ENABLED,null);
			}
		};
	}

}

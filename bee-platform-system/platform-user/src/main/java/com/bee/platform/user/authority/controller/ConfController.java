package com.bee.platform.user.authority.controller;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.user.authority.entity.AuthEnterprise;
import com.bee.platform.user.authority.entity.AuthEnterpriseRole;
import com.bee.platform.user.authority.entity.AuthUserRole;
import com.bee.platform.user.authority.service.AuthEnterpriseRoleService;
import com.bee.platform.user.authority.service.AuthEnterpriseService;
import com.bee.platform.user.authority.service.AuthUserRoleService;
import com.bee.platform.user.entity.UserRole;
import com.bee.platform.user.service.UsersRolesService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.ArrayList;
import java.util.List;

@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "SystemConfig", tags = "获取权限管理系统的公共配置")
@RequestMapping("/authPlatformConf")
public class ConfController {

	@Autowired
	private ConfigService configService;

	/*@Autowired
	private AuthEnterpriseService enterpriseService;

	@Autowired
	private AuthEnterpriseRoleService enterpriseRoleService;

	@Autowired
	private AuthUserRoleService authUserRoleService;

	@Autowired
	private UsersRolesService usersRolesService;*/

	@PostMapping("/getSystemConf")
	@ApiOperation(value = "获取公共配置表接口", notes = "公共配置表接口")
	@ApiImplicitParams({ @ApiImplicitParam(name = "confKey", value = "配置的键", required = true, dataType = "String"),
			@ApiImplicitParam(name = "confValue", value = "配置的值", required = false, dataType = "String"),
			@ApiImplicitParam(name = "confDesc", value = "配置的描述信息", required = false, dataType = "String") })
	public ResponseResult<String> getSystemConf(String confKey, String confValue, String confDesc) {
		String defaultPassword = configService.getConfValue(confKey, confValue, confDesc);
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS.code, ResCodeEnum.SUCCESS.msg, defaultPassword);
	}


	/*@PostMapping("/test")
	@ApiOperation(value = "企业角色迁移开始", notes = "企业角色迁移开始")
	public ResponseResult<String> test() {
		List<UserRole> list=usersRolesService.selectList(new EntityWrapper<UserRole>().where("role_id=1"));
		AuthUserRole userRoles=authUserRoleService.selectOne(new EntityWrapper<AuthUserRole>().where("user_id=1 and deleted=1"));
		List<AuthUserRole> now=new ArrayList<>();
		for (UserRole item:list) {
			AuthUserRole role=BeanUtils.copyProperties(userRoles,AuthUserRole.class)
					.setUserId(item.getUserId()).setEnterpriseId(item.getOrgId())
					.setId(null).setDeleted(0);
			if(item.getUserId()!=1){
				now.add(role);
			}
		}
		    int num=1;
	  		for (AuthUserRole item:now) {
	  			log.info("新增企业角色{}条",num++);
				authUserRoleService.insert(item);
	  		}
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
	}*/


	/** 企业新增中台角色
	 * List<AuthEnterpriseRole> list= enterpriseRoleService.selectList(new EntityWrapper<AuthEnterpriseRole>().where("enterprise_id=634"));
	 * 		List<AuthEnterprise> enterprises=enterpriseService.selectList(new EntityWrapper<>());
	 * 		List<AuthEnterpriseRole> now=new ArrayList<>();
	 * 		for (AuthEnterprise item: enterprises) {
	 * 			int orgId=item.getId();
	 * 			for (AuthEnterpriseRole obj:list) {
	 * 				obj.setEnterpriseId(item.getId()).setId(null);
	 * 				AuthEnterpriseRole role= BeanUtils.copyProperties(obj,AuthEnterpriseRole.class)
	 * 						.setEnterpriseId(orgId).setId(null);
	 * 				if(orgId!=634){
	 * 					now.add(role);
	 *                                }* 			}
	 * 		}
	 * 		int num=1;
	 * 		for (AuthEnterpriseRole item:now) {
	 * 			log.info("新增企业角色{}条",num++);
	 * 			enterpriseRoleService.insert(item);
	 * 		}
	 */
}

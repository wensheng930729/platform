package com.bee.platform.user.controller;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.Validator;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.EnterprisesUsersDTO;
import com.bee.platform.user.dto.EnterprisesUsersInfoDTO;
import com.bee.platform.user.dto.EnterprisesUsersListDTO;
import com.bee.platform.user.dto.EnterprisesUsersRQ;
import com.bee.platform.user.entity.EnterprisesUsers;
import com.bee.platform.user.service.EnterprisesUsersService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 * 前端控制器
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
@Controller

@Slf4j
@Api(value = "企业用户-API", tags = "企业用户相关API")
@RestController
@RequestMapping("/api/enterprisesUsers")
public class EnterprisesUsersController {
	@Autowired
	private AuthPlatformUserService usersService;
	@Autowired
	private EnterprisesUsersService enterprisesUsersService;

	@ApiOperation(value = "通过企业与用户id获取相关信息", notes = "通过企业与用户id获取相关信息")
	@RequestMapping(value = "/getEnterpriseUserInfoById", method = RequestMethod.GET)
	public ResponseResult<EnterprisesUsersDTO> getEnterpriseUserInfoById(@RequestParam Integer userId,
			@RequestParam Integer enterpriseId) {
		return enterprisesUsersService.getEnterpriseUserInfoById(userId, enterpriseId);
	}

	@ApiOperation(value = "添加企业用户", notes = "添加企业用户")
	@RequestMapping(value = "/addEnterprisesUsers", method = RequestMethod.PUT)
	public ResponseResult<EnterprisesUsers> addEnterprisesUsers(
			HttpServletRequest request,@RequestBody  @Valid EnterprisesUsersInfoDTO enterprisesUsersInfoDTO) {
        AuthPlatformUserInfo userInfo = usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
		}
		enterprisesUsersService.add(enterprisesUsersInfoDTO, userInfo);
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
	}
	
	@ApiOperation(value = "外部服务企业用户添加", notes = "外部服务企业用户添加")
	@RequestMapping(value = "/add", method = RequestMethod.PUT)
	public ResponseResult<EnterprisesUsers> add(HttpServletRequest request,String sysToken,  @RequestBody  @Valid EnterprisesUsersInfoDTO enterprisesUsersInfoDTO) {
		if (StringUtils.isEmpty(sysToken)) {
			sysToken = request.getHeader("sysToken");
		}
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(sysToken);
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
		}
		enterprisesUsersService.add(enterprisesUsersInfoDTO, userInfo);
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
	}
	
	@ApiOperation(value = "根据电话或姓名职务部门查询企业用户列表", notes = "根据电话或姓名职务部门查询企业用户列表")
	@RequestMapping(value = "/queryEnterprisesUsers/", method = RequestMethod.POST)
	public ResponseResult<ArrayList<Object>> getEnterprisesUsers(HttpServletRequest request,@RequestBody EnterprisesUsersRQ rq,Page page) {
        AuthPlatformUserInfo userInfo = usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
		Pagination pagination = PageUtils.transFromPage(page);
		return enterprisesUsersService.query(userInfo,rq,
				pagination);
	}

	@ApiOperation(value = "根据当前登录人部门查询企业用户列表", notes = "根据当前登录人部门查询企业用户列表")
	@RequestMapping(value = "/queryAllByOrgId", method = RequestMethod.POST)
	public ResponseResult<List<EnterprisesUsersListDTO>> queryAllByOrgId(HttpServletRequest request, @RequestBody Page page) {
        AuthPlatformUserInfo userInfo = usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
		}
		Pagination pagination = PageUtils.transFromPage(page);
		return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
				enterprisesUsersService.queryAll(userInfo, pagination), PageUtils.transToPage(pagination));

	}

    @ApiOperation(value = "根据企业用户ID设置用户是否启用禁用", notes = "根据企业用户ID设置用户是否启用禁用")
    @RequestMapping(value = "/upDateById", method = RequestMethod.POST)
    public ResponseResult<ResponseResult<Object>> upDateById(HttpServletRequest request,int id, Integer isActive) {
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
		if (ObjectUtils.isEmpty(userInfo)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
		}
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,enterprisesUsersService.upDateById(id, isActive, userInfo));

    }
    
    @ApiOperation(value = "根据企业用户ID编辑企业用户", notes = "根据企业用户ID编辑企业用户")
    @RequestMapping(value = "/upDateByIdEnterprisesUsers", method = RequestMethod.POST)
    public ResponseResult<ResponseResult<Object>> updateEnterprisesUsers(HttpServletRequest request, @RequestBody EnterprisesUsersInfoDTO enterprisesUsersInfoDTO) {
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN, request));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,enterprisesUsersService.upDateByIdEnterprisesUsers(userInfo, enterprisesUsersInfoDTO));

    }
    
    @ApiOperation(value = "根据电话判断企业用户是否被注册", notes = "根据电话判断企业用户是否被注册")
    @RequestMapping(value = "/getEnterprisesUsers", method = RequestMethod.POST)
    public ResponseResult<ResponseResult<Object>> getEnterprisesUsers(String phone){
    	if (!Validator.isMobile(phone)) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
		}
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,enterprisesUsersService.getEnterprisesUsers(phone));

    }
    
    @ApiOperation(value = "根据企业用户ID查询一条数据", notes = "根据企业用户ID查询一条数据")
    @RequestMapping(value = "/getEnterprisesUsersById", method = RequestMethod.POST)
    public ResponseResult<Object> getEnterprisesUsersById(Integer id){
		if (id==null) {
			return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_COMPANY_ID);
		}
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,enterprisesUsersService.getEnterprisesUsersById(id));

    }
}

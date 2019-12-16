package com.bee.platform.user.controller;


import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.service.EnterprisesCheckService;
import com.bee.platform.user.service.EnterprisesService;
import com.bee.platform.user.service.EnterprisesUsersService;
import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "enterprises", tags = "企业相关接口")
@RequestMapping("/api/enterprise")
public class EnterprisesController {

    @Autowired
    private EnterprisesCheckService enterprisesCheckService;

    @Autowired
    private EnterprisesUsersService enterprisesUsersService;

    @Autowired
    private EnterprisesService enterprisesService;

    @Autowired
    private AuthPlatformUserService usersService;

    private final static String SYS_TOKEN = "sysToken";

/*
    @ApiOperation(value = "企业注册")
    @RequestMapping(value = "/register", method = RequestMethod.POST)
    public ResponseResult register(HttpServletRequest request , @RequestBody() @Valid EnterpriseRegisterInfoRQ rq) {
        if(ObjectUtils.isEmpty(rq)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())||ObjectUtils.isEmpty(userInfo.getNickname())){
            log.error("获取用户信息失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        return enterprisesCheckService.register(userInfo,rq);
    }

    @ApiOperation(value = "企业申请名称校验")
    @GetMapping("/enterpriseCheck")
    public ResponseResult enterpriseCheck(HttpServletRequest request ,@RequestParam() String name){
        if(StringUtils.isEmpty(name)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = usersService.getSelfInfo(request.getHeader(SYS_TOKEN));
        if(ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())){
            log.error("获取用户信息失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return  enterprisesService.enterpriseCheck(userInfo,name);
    }

    @ApiOperation(value = "模糊查询企业列表")
    @GetMapping("/searchEnterpriseList")
    public ResponseResult searchEnterpriseList(@RequestParam() String name, Page page){
        if(StringUtils.isEmpty(name)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return enterprisesService.searchEnterpriseList(name,page);
    }


    @ApiOperation(value = "获取企业信息")
    @RequestMapping(value = "/info", method = RequestMethod.GET)
    public ResponseResult<EnterpriseDetailDTO> getEnterpriseInfo(HttpServletRequest request) {
        String sysToken=request.getHeader(SYS_TOKEN);
        AuthPlatformUserInfo userInfo=usersService.getUserInfo(sysToken);
        Integer orgId = userInfo.getOrgId();
        if (enterprisesUsersService.findByUserIdAndEnterpriseId(userInfo.getId(), orgId) == null) {
            return ResponseResult.fail("当前用户不属于该部门");
        }
        return enterprisesService.getById(orgId);
    }

    @ApiOperation(value = "获取企业信息")
    @RequestMapping(value = "/company", method = RequestMethod.POST)
    public ResponseResult<List<CompanyDTO>> getCompanyInfo(@RequestBody @Valid EnterpriseDTO param) {
        List<CompanyDTO> companies = new ArrayList<>();
        try {
            List<Enterprises> list=enterprisesService.selectBatchIds(param.getOrgId());
            if(!CollectionUtils.isEmpty(list)){
                List<EnterpriseDetailDTO> dtos= BeanUtils.assemble(EnterpriseDetailDTO.class,list);
                companies=dtos.stream().map(obj->{

                    CompanyDTO dto= new CompanyDTO(obj);
                    //行业信息
                    IndustryDTO industry=obj.getIndustry();
                    if(industry!=null && industry.getChildIndustry()!=null){
                        dto.setIndustry(industry.getChildIndustry().getIndustry());
                    }
                    return dto;
                }).collect(Collectors.toList());
            }
        } catch (Exception e) {
            log.error("获取企业信息失败", e);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,companies);
    }

    @ApiOperation(value = "通过名称获得企业信息")
    @RequestMapping(value = "/companyName", method = RequestMethod.POST)
    public ResponseResult<List<CompanyDTO>> getCompany(HttpServletRequest request, @RequestBody CompanyListDTO dto) {
        List<String> names=dto.getCompanyName();
        Preconditions.checkArgument(!CollectionUtils.isEmpty(names) && StringUtils.isNotBlank(dto.getPassword()), "参数错误");
        List<Enterprises> dataList = enterprisesService.listByNames(names);
        List<CompanyDTO> companies=new ArrayList<>();
        if(!CollectionUtils.isEmpty(dataList)){
            List<EnterpriseDetailDTO> dtos= BeanUtils.assemble(EnterpriseDetailDTO.class,dataList);
            for (EnterpriseDetailDTO item:dtos) {
                CompanyDTO company= new CompanyDTO(item);
                //行业信息
                IndustryDTO industry=item.getIndustry();
                if(industry!=null && industry.getChildIndustry()!=null){
                    company.setIndustry(industry.getChildIndustry().getIndustry());
                }
                companies.add(company);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,companies);
    }

    @ApiOperation(value = "获取企业管理员")
    @RequestMapping(value = "/admins", method = RequestMethod.GET)
    public ResponseResult<List<AdminDTO>> getAdmins(HttpServletRequest request) {
        String sysToken=request.getHeader(SYS_TOKEN);
        AuthPlatformUserInfo userInfo=usersService.getUserInfo(sysToken);
        List<AuthUserRoleInfo> roleInfoList = userInfo.getRoleList()
                .stream()
                .filter(a -> a.getRoleType().equals(AuthRoleType.SUPER_ADMIN.getDesc()))
                .collect(Collectors.toList());
        if (CollectionUtils.isEmpty(roleInfoList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_AUTHORITY);
        }
        List<AdminDTO> adminList = enterprisesService.getAdmins(userInfo.getOrgId());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,adminList);
    }

    @ApiOperation(value = "添加管理员")
    @ApiImplicitParam(name = "id", value = "用户id", required = true, dataType = "int", paramType = "path")
    @RequestMapping(value = "/user/{id}", method = RequestMethod.POST)
    public ResponseResult<String> addAdmins(HttpServletRequest request, @PathVariable int id) {
        String sysToken=request.getHeader(SYS_TOKEN);
        AuthPlatformUserInfo userInfo=usersService.getUserInfo(sysToken);
        List<AuthUserRoleInfo> roleInfoList = userInfo.getRoleList()
                .stream()
                .filter(a -> a.getRoleType().equals(AuthRoleType.SUPER_ADMIN.getDesc()))
                .collect(Collectors.toList());
        if (CollectionUtils.isEmpty(roleInfoList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_AUTHORITY);
        }
        return enterprisesService.addAdmins(userInfo.getOrgId(), id);
    }

    @ApiOperation(value = "批量添加管理员")
    @RequestMapping(value = "/admins/add", method = RequestMethod.POST)
    public ResponseResult<String> addAdmins(HttpServletRequest request, @RequestParam int[] old_ids, @RequestParam int[] ids) {
        String sysToken=request.getHeader(SYS_TOKEN);
        AuthPlatformUserInfo userInfo=usersService.getUserInfo(sysToken);
        if(ObjectUtils.isEmpty(ids)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return enterprisesService.addAdminsByList(userInfo.getOrgId(),old_ids, ids);
    }

    @ApiOperation(value = "删除管理员")
    @ApiImplicitParam(name = "id", value = "用户id", required = true, dataType = "int", paramType = "path")
    @RequestMapping(value = "/user/modify/{id}", method = RequestMethod.POST)
    public ResponseResult<String> deleteAdmins(HttpServletRequest request, @PathVariable int id) {
        String sysToken=request.getHeader(SYS_TOKEN);
        AuthPlatformUserInfo userInfo=usersService.getUserInfo(sysToken);
        List<AuthUserRoleInfo> roleInfoList = userInfo.getRoleList()
                .stream()
                .filter(a -> a.getRoleType().equals(AuthRoleType.SUPER_ADMIN.getDesc()))
                .collect(Collectors.toList());
        if (CollectionUtils.isEmpty(roleInfoList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_AUTHORITY);
        }
        return enterprisesService.deleteAdmins(userInfo.getOrgId(), id);
    }

    @ApiOperation(value = "批量删除管理员")
    @RequestMapping(value = "/admins/delete", method = RequestMethod.POST)
    public ResponseResult<String> deleteAdmins(HttpServletRequest request, @RequestParam int[] ids) {
        String sysToken=request.getHeader(SYS_TOKEN);
        AuthPlatformUserInfo userInfo=usersService.getUserInfo(sysToken);
        List<AuthUserRoleInfo> roleInfoList = userInfo.getRoleList()
                .stream()
                .filter(a -> a.getRoleType().equals(AuthRoleType.SUPER_ADMIN.getDesc()))
                .collect(Collectors.toList());
        if (CollectionUtils.isEmpty(roleInfoList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_AUTHORITY);
        }
        return enterprisesService.deleteAdminsByList(userInfo.getOrgId(), ids);
    }

    @ApiOperation(value = "获取用户的企业列表")
    @RequestMapping(method = RequestMethod.GET)
    public ResponseResult<List<EnterpriseListDTO>> getEnterpriseList(HttpServletRequest request) {
        AuthPlatformUserInfo userInfo=usersService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));
        return enterprisesService.getAllEnterprise(userInfo);
    }

    @ApiOperation(value = "获取用户作为管理员的企业列表")
    @RequestMapping(value = "/admin", method = RequestMethod.GET)
    public ResponseResult<List<EnterpriseListDTO>> getEnterpriseListByAdmin(HttpServletRequest request) {
        AuthPlatformUserInfo userInfo=usersService.getSelfInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));
        return enterprisesService.getAllEnterpriseByAdmin(userInfo);
    }

    @ApiOperation(value = "修改企业头像")
    @RequestMapping(value = "/modify_head", method = RequestMethod.POST)
    public ResponseResult<String> modifyEnterpriseHead(HttpServletRequest request, @RequestParam String head) {
        AuthPlatformUserInfo userInfo=usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));
        List<AuthUserRoleInfo> roleInfoList = userInfo.getRoleList()
                .stream()
                .filter(a -> a.getRoleType().equals(AuthRoleType.SUPER_ADMIN.getDesc()))
                .collect(Collectors.toList());
        if (CollectionUtils.isEmpty(roleInfoList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_AUTHORITY);
        }
        if (StringUtils.isEmpty(head)){
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_HEAD_URL);
        }
        return enterprisesService.modifyEnterpriseHead(userInfo.getOrgId(), head);
    }

    @ApiOperation(value = "转让超级管理员")
    @RequestMapping(value = "/transferAdmin", method = RequestMethod.POST)
    public ResponseResult<String> transferAdmin(HttpServletRequest request, @RequestParam String phone) {
        AuthPlatformUserInfo userInfo=usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));
        List<AuthUserRoleInfo> roleInfoList = userInfo.getRoleList()
                .stream()
                .filter(a -> a.getRoleType().equals(AuthRoleType.SUPER_ADMIN.getDesc()))
                .collect(Collectors.toList());
        if (CollectionUtils.isEmpty(roleInfoList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_AUTHORITY);
        }

        return enterprisesService.transferAdmin(userInfo, phone);
    }

    @ApiOperation(value = "根据企业id查询企业下成员信息")
    @RequestMapping(value = "/list/users", method = RequestMethod.POST)
    public ResponseResult<List<UserManagementDTO>> listUsersByEnterpriseId(HttpServletRequest request,@RequestBody @Valid UserManagementVO vo){
        AuthPlatformUserInfo userInfo=usersService.getUserInfo(WebUtils.getParam(ConstantsUtil.SYS_TOKEN,request));
        return enterprisesService.listUsersByEnterpriseId(userInfo.getOrgId(),vo);
    }

    @ApiOperation(value = "查询是否是已注册企业")
    @RequestMapping(value = "/registeredEnterprise", method = RequestMethod.GET)
    public ResponseResult<List<EnterpriseInfoDTO>> getRegisteredEnterprise(HttpServletRequest request, @RequestParam String enterpriseName) {
        return enterprisesService.listRegisteredEnterpriseInfo(enterpriseName);
    }

    @ApiOperation(value = "获得用户所在企业的基本信息")
    @RequestMapping(value = "/basic/info", method = RequestMethod.GET)
    public ResponseResult<List<EnterpriseBasicDTO>> listBasicInfo(@RequestHeader("sysToken")String sysToken){
        AuthPlatformUserInfo userInfo=usersService.getSelfInfo(sysToken);
        return enterprisesService.listBasicInfo(userInfo.getId());
    }


*/




}


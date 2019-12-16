package com.bee.platform.user.controller;


import com.bee.platform.business.dto.DepartmentDTO;
import com.bee.platform.business.dto.DepartmentListDTO;
import com.bee.platform.business.dto.DepartmentTreePostDTO;
import com.bee.platform.business.rq.DepartmentAddRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.authority.dto.AuthUsergroupDeparmentTreeDTO;
import com.bee.platform.user.authority.dto.AuthUsergroupUserListDTO;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.dto.DepartmentTreeDTO;
import com.bee.platform.user.service.DepartmentsService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Set;

/**
 * <p>
 * 前端控制器
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "departments", tags = "部门相关接口")
@RequestMapping("/api/enterprise/department")
public class DepartmentsController {

    private final static String SYS_TOKEN = "sysToken";

    @Autowired
    private DepartmentsService departmentsService;
    @Autowired
    private AuthPlatformUserService authPlatformUserService;
    @Autowired
    private AuthPlatformUserService userService;

    @ApiOperation(value = "新版 获取部门层级树")
    @GetMapping(value = "/getDepartmentTree")
    public ResponseResult<List<DepartmentDTO>> getDepartmentTree(HttpServletRequest request, Integer level) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(request.getHeader(SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return departmentsService.getDepartmentTree(userInfo, level);
    }

    @ApiOperation(value = "新版 获取部门层级树 职位用")
    @GetMapping(value = "/getDepartmentTreeForPost")
    public ResponseResult<List<DepartmentTreePostDTO>> getDepartmentTreeForPost(HttpServletRequest request) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(request.getHeader(SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return departmentsService.getDepartmentTreeForPost(userInfo);
    }

    @ApiOperation(value = "新版 条件查询部门列表")
    @GetMapping(value = "/getDepartmentList")
    public ResponseResult<List<DepartmentListDTO>> getDepartmentList(HttpServletRequest request, String name, Page page) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(request.getHeader(SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return departmentsService.getDepartmentList(userInfo, name, page);
    }

    @ApiOperation(value = "新版 编辑 新增部门", notes = "编辑 新增部门,企业管理员有此权限")
    @PostMapping(value = "/editDepartment")
    public ResponseResult editDepartment(HttpServletRequest request, @RequestBody DepartmentAddRQ rq) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(request.getHeader(SYS_TOKEN));
        if(ObjectUtils.isEmpty(rq)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        if (ObjectUtils.isEmpty(rq.getDepartmentId())) {
            return departmentsService.add(userInfo, rq);
        } else {
            return departmentsService.editDepartment(userInfo, rq);
        }
    }

    @ApiOperation(value = "新版删除部门", notes = "添加部门,企业管理员有此权限")
    @PostMapping(value = "/deletePost")
    public ResponseResult deletePost(HttpServletRequest request, Integer departmentId) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(request.getHeader(SYS_TOKEN));
        return departmentsService.deleteDepartment(userInfo, departmentId);
    }
    
    @ApiOperation(value = "内部服务获取", notes = "获取用户下属列表")
    @GetMapping(value = "/subordinates")
    public ResponseResult<Set<Integer>> subordinates(@RequestParam Integer orgId, 
        @RequestParam Integer userId) {
        return departmentsService.getSubordinates(userId, orgId);
    }

    @ApiOperation(value = "部门验证名称唯一性 0存在 1不存在", notes = "部门验证名称唯一性")
    @RequestMapping(value = "/departmentNameCheck", method = RequestMethod.GET)
    public ResponseResult<Integer> departmentNameCheck(HttpServletRequest request, @RequestParam String name,@RequestParam(required = false) Integer treeId,
                                                       @RequestParam Integer level, @RequestParam(required = false) Integer departmentId) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(request.getHeader(SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return departmentsService.departmentNameCheck(userInfo, name, level, departmentId, treeId);
    }

    @ApiOperation(value = "通过企业id查询部门")
    @GetMapping(value = "/get/{enterpriseId}")
    ResponseResult<List<DepartmentTreeDTO>> getByEnterpriseId(@RequestHeader("sysToken") String sysToken, @PathVariable("enterpriseId") Integer enterpriseId) {
        AuthPlatformUserInfo userInfo = authPlatformUserService.getSelfInfo(sysToken);
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        List<DepartmentTreeDTO> list=departmentsService.departmentTree(enterpriseId,userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
    }

    @ApiOperation(value = "用户组--部门树")
    @GetMapping(value = "/usergroup/deparmentTree")
    public ResponseResult<AuthUsergroupDeparmentTreeDTO> getUsergroupDeparmentTree(HttpServletRequest request){
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return departmentsService.getUsergroupDeparmentTree(userInfo.getOrgId());
    }

    @ApiOperation(value = "用户组--根据部门id查询用户")
    @GetMapping(value = "/usergroup/deparmentUsers/{id}")
    public ResponseResult<List<AuthUsergroupUserListDTO>> getUsergroupDeparmentUsers(HttpServletRequest request,@PathVariable("id") int id){
        AuthPlatformUserInfo userInfo = userService.getSelfInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USERINFO);
        }
        return departmentsService.getUsergroupDeparmentUsers(userInfo.getOrgId(),id);
    }

}


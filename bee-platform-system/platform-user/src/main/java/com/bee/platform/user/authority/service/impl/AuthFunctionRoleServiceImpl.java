package com.bee.platform.user.authority.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.EnumRoleType;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.user.authority.dao.mapper.AuthEnterpriseRoleMapper;
import com.bee.platform.user.authority.dao.mapper.AuthFunctionRoleMapper;
import com.bee.platform.user.authority.dto.AuthRoleTreeDTO;
import com.bee.platform.user.authority.dto.AuthRoleUsedDTO;
import com.bee.platform.user.authority.entity.AuthEnterprise;
import com.bee.platform.user.authority.entity.AuthEnterpriseRole;
import com.bee.platform.user.authority.entity.AuthFunctionRole;
import com.bee.platform.user.authority.entity.AuthRole;
import com.bee.platform.user.authority.enums.EnumNewCommon;
import com.bee.platform.user.authority.rq.AuthEnterpriseRoleConfigRQ;
import com.bee.platform.user.authority.rq.EnterpriseRelationRoleRQ;
import com.bee.platform.user.authority.rq.RoleRelationRoleRQ;
import com.bee.platform.user.authority.service.AuthEnterpriseRoleService;
import com.bee.platform.user.authority.service.AuthEnterpriseService;
import com.bee.platform.user.authority.service.AuthFunctionRoleService;
import com.bee.platform.user.authority.service.AuthRoleService;
import com.bee.platform.user.dto.AuthEnterpriseAppFunDTO;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * <p>
 * 功能关联角色的中间表 服务实现类
 * </p>
 *
 * @author cheng.ke
 * @since 2019-05-20
 */
@Slf4j
@Service
public class AuthFunctionRoleServiceImpl extends ServiceImpl<AuthFunctionRoleMapper, AuthFunctionRole> implements AuthFunctionRoleService {

    @Autowired
    private AuthRoleService roleService;
    @Autowired
    private AuthFunctionRoleService functionRoleService;
    @Autowired
    private AuthEnterpriseService enterpriseService;
    @Autowired
    private AuthEnterpriseRoleService enterpriseRoleService;
    @Autowired
    private AuthEnterpriseRoleMapper enterpriseRoleMapper;


    /**
     * 角色<-->功能<-->应用对应关系变更方法
     *
     * @param rq 请求参数
     * @return 操作结果
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void changeFunctionRole(Integer userId, RoleRelationRoleRQ rq) {
        Date time = new Date();
        List<Integer> childIds = rq.getChildIds();
        Integer pId = rq.getParentId();
        Integer type = rq.getType();
        switch (type) {
            case 0:
                // 添加角色功能应用对应关系
                saveFunctionRole(userId, time, childIds, pId);
                break;
            case 1:
                // 删除角色功能应用关联关系
                deleteFunctionRole(time, pId);
                // 添加角色功能应用对应关系
                saveFunctionRole(userId, time, childIds, pId);
                break;
            case 2:
                // 删除角色功能应用关联关系
                deleteFunctionRole(time, pId);
                break;
            default:
                throw new BusinessException(ResCodeEnum.FAILED, ExceptionMessageEnum.FUNCTION_ROLE_UPDATE_FAILED);

        }

    }

    /**
     * 删除角色功能应用对应关系
     *
     * @param time 修改时间
     * @param pId  父级id
     */
    private void deleteFunctionRole(Date time, Integer pId) {
        if (!this.update(new AuthFunctionRole().setDeleted(1).setUpdateTime(time), new EntityWrapper<AuthFunctionRole>().eq("function_id", pId).eq("deleted", 0))) {
            log.error("修改功能、应用失败，调用{}的{}方法出错", "AuthFunctionRoleServiceImpl", "deleteFunctionRole()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.FUNCTION_ROLE_DELETE_FAILED);
        }
    }

    /**
     * 添加角色功能应用对应关系
     *
     * @param userId   创建人id
     * @param time     创建时间
     * @param childIds 子id集合
     * @param pId      父id
     */
    private void saveFunctionRole(Integer userId, Date time, List<Integer> childIds, Integer pId) {
        if (CollectionUtils.isEmpty(childIds)) {
            log.error("添加功能、应用失败，调用{}的{}方法出错", "AuthFunctionRoleServiceImpl", "saveFunctionRole()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.FUNCTION_ROLE_SAVE_FAILED);
        }

        // 添加角色功能应用关联关系
        for (Integer childId : childIds) {
            // 查询是否存在该角色
            AuthRole authRole = roleService.selectOne(new EntityWrapper<AuthRole>().eq("id", childId).eq("deleted", 0));
            // 查询是否关联过该功能
            AuthFunctionRole functionRole = selectOne(new EntityWrapper<AuthFunctionRole>().eq("function_id", pId).eq("role_id", childId).eq("deleted", 0));
            if (!ObjectUtils.isEmpty(authRole) && ObjectUtils.isEmpty(functionRole)) {
                AuthFunctionRole authFunctionRole = new AuthFunctionRole().setPid(pId).setRoleId(childId).setCreateTime(time)
                        .setUpdateTime(time).setDeleted(0).setStatus(1).setCreateUser(userId);
                if (!insert(authFunctionRole)) {
                    log.error("添加功能、应用失败，调用{}的{}方法出错", "AuthFunctionRoleServiceImpl", "saveFunctionRole()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.FUNCTION_ROLE_SAVE_FAILED);
                }
            }
        }
    }

    /**
     * 查询当前用户所在企业拥有的功能 中台
     */
    @Override
    public ResponseResult<List<AuthRoleTreeDTO>> getEnterpriseFuns(Integer enterpriseId) {
        AuthEnterprise enterprise = enterpriseService.selectOne(new EntityWrapper<>(new AuthEnterprise()
                .setId(enterpriseId)
                .setStatus(EnumNewCommon.IsActive.is_active.getKey())
                .setDeleted(EnumNewCommon.IsDeleted.not_Delete.getKey())));
        if (ObjectUtils.isEmpty(enterprise)) {
            log.error("企业信息不存在，类是：{}，方法是：{}", "AuthEnterpriseServiceImpl", "updateEnterprise");
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        // 查询企业开通的应用和功能一、二
        List<AuthEnterpriseRole> enterpriseRoles = enterpriseRoleService.selectList(new EntityWrapper<>(new AuthEnterpriseRole()
                .setEnterpriseId(enterprise.getId())
                .setDeleted(EnumNewCommon.IsDeleted.not_Delete.getKey()))
                .eq("role_type", EnumRoleType.FUNCTION_ONE.getCode())
                .or().eq("role_type", EnumRoleType.FUNCTION_TWO.getCode()));
        if (CollectionUtils.isEmpty(enterpriseRoles)) {
            log.error("该企业什么都没开通！类是：{}，方法是：{}", "AuthEnterpriseServiceImpl", "getEnterFuns");
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(1));
        }
        // 应用的ids
        Set<Integer> appIds = enterpriseRoles.stream()
                .filter(a -> a.getRoleType().equals(EnumRoleType.FUNCTION_ONE.getCode()))
                .map(a -> a.getPid())
                .collect(Collectors.toSet());
        // 功能的ids
        Set<Integer> funIds = enterpriseRoles.stream().map(a -> a.getRoleId()).collect(Collectors.toSet());
        appIds.addAll(funIds);
        // 角色ids查询所有的角色
        List<AuthRole> roles = roleService.selectList(new EntityWrapper<>(new AuthRole()
                .setDeleted(EnumNewCommon.IsDeleted.not_Delete.getKey()))
                .in("id", appIds));
        List<AuthRoleTreeDTO> roleDtos = BeanUtils.assemble(AuthRoleTreeDTO.class, roles);
        // 应用s
        List<AuthRoleTreeDTO> apps = roleDtos.stream().filter(a -> a.getRoleType().equals(EnumRoleType.APPLICATION.getCode())).collect(Collectors.toList());
        for (AuthRoleTreeDTO app : apps) {
            app.setPid(0);
            buildApp(app, enterpriseRoles, roleDtos);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, apps);
    }

    private AuthRoleTreeDTO buildApp(AuthRoleTreeDTO app, List<AuthEnterpriseRole> enterpriseRoles, List<AuthRoleTreeDTO> roleDtos) {
        for (AuthRoleTreeDTO roleDto : roleDtos) {
            for (AuthEnterpriseRole enterpriseRole : enterpriseRoles) {
                if (enterpriseRole.getPid().equals(app.getId())
                        && enterpriseRole.getRoleId().equals(roleDto.getId())) {
                    if (app.getChildren() == null) {
                        app.setChildren(Lists.newArrayList());
                    }
                    AuthRoleTreeDTO dtoCopy = BeanUtils.copyProperties(roleDto, AuthRoleTreeDTO.class).setPid(app.getId());
                    app.getChildren().add(buildApp(dtoCopy, enterpriseRoles, roleDtos));
                }
            }
        }
        return app;
    }

    /**
     * 查询所有应用下的所有功能 后台
     */
    @Override
    public ResponseResult<List<AuthEnterpriseAppFunDTO>> getAllFuns() {
        List<AuthRole> roleList = roleService.selectList(new EntityWrapper<>(new AuthRole()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()))
                .ne("sub_sys", "bee_console"));
        List<AuthEnterpriseAppFunDTO> roleDtoList = BeanUtils.assemble(AuthEnterpriseAppFunDTO.class, roleList);

        List<AuthEnterpriseAppFunDTO> appList = roleDtoList.stream().filter(a -> a.getRoleType().equals(EnumRoleType.APPLICATION.getCode())).collect(Collectors.toList());
        List<AuthEnterpriseAppFunDTO> funOneList = roleDtoList.stream().filter(a -> a.getRoleType().equals(EnumRoleType.FUNCTION_ONE.getCode())).collect(Collectors.toList());
        List<AuthEnterpriseAppFunDTO> funTwoList = roleDtoList.stream().filter(a -> a.getRoleType().equals(EnumRoleType.FUNCTION_TWO.getCode())).collect(Collectors.toList());

        List<AuthFunctionRole> functionRoleList = functionRoleService.selectList(new EntityWrapper<>(new AuthFunctionRole()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())));
        // 获取功能一下面的功能二
        buildFunTree(funOneList, funTwoList, functionRoleList);
        // 获取app下的功能一
        buildFunTree(appList, funOneList, functionRoleList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, appList);
    }

    private void buildFunTree(List<AuthEnterpriseAppFunDTO> oneList, List<AuthEnterpriseAppFunDTO> twoList, List<AuthFunctionRole> functionRoleList) {
        for (AuthEnterpriseAppFunDTO one : oneList) {
            if (one.getRoleType().equals(EnumRoleType.APPLICATION.getCode())) {
                one.setPid(0);
            }
            for (AuthEnterpriseAppFunDTO two : twoList) {
                for (AuthFunctionRole functionRole : functionRoleList) {
                    if (functionRole.getRoleId().equals(two.getId()) && functionRole.getPid().equals(one.getId())) {
                        if (CollectionUtils.isEmpty(one.getChildren())) {
                            one.setChildren(Lists.newArrayList());
                        }
                        AuthEnterpriseAppFunDTO dto = BeanUtils.copyProperties(two, AuthEnterpriseAppFunDTO.class).setPid(one.getId());
                        one.getChildren().add(dto);
                    }
                }
            }
        }
    }

    /**
     * 通过用户查询企业已开通哪些功能
     */
    @Override
    public ResponseResult<List<AuthRoleUsedDTO>> getEnterpriseFunsByEnterpriseId(Integer enterpriseId) {
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,
                enterpriseRoleMapper.getEnterpriseFunsByEnterpriseId(enterpriseId));
    }


    /**
     * 查询二级功能下的基础角色ids
     */
    @Override
    public List<AuthFunctionRole> getFunTwoFollowRole(List<Integer> twoIds) {
        return functionRoleService.selectList(new EntityWrapper<>(new AuthFunctionRole()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())).in("pid", twoIds));
    }

    /**
     * 保存企业权限配置的修改
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult changeEnterpriseRole(AuthEnterpriseRoleConfigRQ rq, AuthPlatformUserInfo userInfo) {
        if (ObjectUtils.isEmpty(rq)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        List<EnterpriseRelationRoleRQ> list = rq.getList();

        if (CollectionUtils.isEmpty(list)) {
            // 如果为空则 逻辑删除历史数据
            enterpriseRoleService.update(new AuthEnterpriseRole()
                            .setDeleted(EnumCommon.IsDeleted.is_Delete.getKey())
                            .setUpdateTime(new Date()),
                    new EntityWrapper<>(new AuthEnterpriseRole()
                            .setEnterpriseId(rq.getEnterpriseId())));
        } else {
            // 否则先删除再添加
            Integer enterpriseId = rq.getEnterpriseId();
            enterpriseRoleService.update(new AuthEnterpriseRole()
                            .setDeleted(EnumCommon.IsDeleted.is_Delete.getKey())
                            .setUpdateTime(new Date()),
                    new EntityWrapper<>(new AuthEnterpriseRole()
                            .setEnterpriseId(enterpriseId)));
            List<AuthEnterpriseRole> addList = BeanUtils.assemble(AuthEnterpriseRole.class, list);
            for (AuthEnterpriseRole enterpriseRole : addList) {
                enterpriseRole.setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                        .setStatus(EnumCommon.IsActive.is_active.getKey())
                        .setCreateUser(userInfo.getId())
                        .setCreateTime(new Date());
            }
            // 给企业插入新的权限角色
            if (!enterpriseRoleService.insertBatch(addList)) {
                log.error("给企业配置权限时失败，类是：{}，方法是：{}", "AuthEnterpriseServiceImpl", "changeEnterpriseRole");
                throw new BusinessException(ResCodeEnum.AUTHORITY_CONFIGURATION_FAILED, ExceptionMessageEnum.AUTHORITY_CONFIGURATION_FAILED);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
}

package com.bee.platform.user.authority.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.authority.dao.mapper.AuthUsergroupMapper;
import com.bee.platform.user.authority.dto.AuthUsergroupDetailDTO;
import com.bee.platform.user.authority.dto.AuthUsergroupListDTO;
import com.bee.platform.user.authority.dto.AuthUsergroupUsersDTO;
import com.bee.platform.user.authority.entity.AuthUsergroup;
import com.bee.platform.user.authority.entity.AuthUsergroupRole;
import com.bee.platform.user.authority.entity.AuthUsergroupUser;
import com.bee.platform.user.authority.rq.AuthUsergroupAddRQ;
import com.bee.platform.user.authority.rq.AuthUsergroupUpdateRQ;
import com.bee.platform.user.authority.rq.AuthUsergroupUsersListRQ;
import com.bee.platform.user.authority.service.AuthUsergroupRoleService;
import com.bee.platform.user.authority.service.AuthUsergroupService;
import com.bee.platform.user.authority.service.AuthUsergroupUserService;
import com.bee.platform.user.entity.Departments;
import com.bee.platform.user.service.DepartmentsService;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 用户组 服务实现类
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-07-17
 */
@Slf4j
@Service
public class AuthUsergroupServiceImpl extends ServiceImpl<AuthUsergroupMapper, AuthUsergroup> implements AuthUsergroupService {

    @Autowired
    private AuthUsergroupMapper usergroupMapper;
    @Autowired
    private AuthUsergroupUserService usergroupUserService;
    @Autowired
    private AuthUsergroupRoleService authUsergroupRoleService;
    @Autowired
    private DepartmentsService departmentsService;


    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<?> addUsergroup(AuthPlatformUserInfo userInfo, AuthUsergroupAddRQ rq) {
        Integer orgId = userInfo.getOrgId();
        // 如果新增用户组是默认，则校验是否已存在默认组
        if (Objects.equals(Status.TRUE.getKey(), rq.getDefaultValue())) {
            AuthUsergroup defaultGroup = queryDefault(orgId);
            if (!ObjectUtils.isEmpty(defaultGroup)) {
                this.updateById(defaultGroup.setDefaultValue(Status.FALSE.getKey()));
            }
        }
        // 组名是否存在
        if (this.checkNameRepeat(orgId, rq.getGroupName())) {
            log.info("用户组名称已存在，不能新增默认用户组！类：{} 方法：{}", "AuthUsergroupServiceImpl", "addUsergroup");
            return ResponseResult.buildResponseResult(ResCodeEnum.NAME_EXIST);
        }
        AuthUsergroup usergroup = BeanUtils.copyProperties(rq, AuthUsergroup.class);
        usergroup.setCreateUser(userInfo.getId())
                .setCreateTime(new Date())
                .setUpdateTime(new Date())
                .setEnterpriseId(orgId)
                .setDeleted(Status.FALSE.getKey());
        if (!this.insert(usergroup)) {
            log.info("新增用户组失败！类：{} 方法：{}", "AuthUsergroupServiceImpl", "addUsergroup");
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 检测用户组名称是否重复
     *
     * @param orgId
     * @param groupName
     * @return
     */
    private boolean checkNameRepeat(int orgId, String groupName) {
        int count = this.selectCount(new EntityWrapper<>(new AuthUsergroup()
                .setDeleted(Status.FALSE.getKey())
                .setGroupName(groupName)
                .setEnterpriseId(orgId)));
        return count > 0;
    }

    /**
     * 检测是否存在默认用户组
     *
     * @param orgId 企业id
     * @return
     */
    private AuthUsergroup queryDefault(int orgId) {
        return this.selectOne(new EntityWrapper<>(new AuthUsergroup()
                .setDeleted(Status.FALSE.getKey())
                .setDefaultValue(Status.TRUE.getKey())
                .setEnterpriseId(orgId)));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<?> deleteUsergroup(Integer id) {
        AuthUsergroup usergroup = usergroupMapper.selectOne(new AuthUsergroup().setId(id).setDeleted(Status.FALSE.getKey()));
        if (ObjectUtils.isEmpty(usergroup)) {
            log.info("用户组不存在！类：{} 方法：{}", "AuthUsergroupServiceImpl", "deleteUsergroup");
            return ResponseResult.buildResponseResult(ResCodeEnum.USERGROUP_NOT_EXIST);
        }
        // 删除用户组
        usergroup.setDeleted(Status.TRUE.getKey());
        if (!this.updateById(usergroup)) {
            log.error("用户组删除失败！类：{} 方法：{}", "AuthUsergroupServiceImpl", "deleteUsergroup");
            return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_FAIL);
        }
        // 删除用户组与用户 中间表
        usergroupUserService.update(new AuthUsergroupUser()
                        .setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<>(new AuthUsergroupUser().setUsergroupId(id)));
        // 删除用户组权限
        authUsergroupRoleService.update(new AuthUsergroupRole()
                        .setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<>(new AuthUsergroupRole().setUsergroupId(id)));

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<?> updateUsergroup(AuthPlatformUserInfo userInfo, AuthUsergroupUpdateRQ rq) {
        Integer orgId = userInfo.getOrgId();
        AuthUsergroup usergroup = usergroupMapper.selectOne(new AuthUsergroup()
                .setId(rq.getId())
                .setDeleted(Status.FALSE.getKey()));
        if (ObjectUtils.isEmpty(usergroup)) {
            log.info("用户组不存在！类：{} 方法：{}", "AuthUsergroupServiceImpl", "updateUsergroup");
            return ResponseResult.buildResponseResult(ResCodeEnum.USERGROUP_NOT_EXIST);
        }
        // 如果用户组原来不是默认的，修改成默认则需要判断---是否有默认值 有则修改原来的非默认
        if (Objects.equals(Status.FALSE.getKey(), usergroup.getDefaultValue()) && Objects.equals(Status.TRUE.getKey(), rq.getDefaultValue())) {
            AuthUsergroup defaultGroup = queryDefault(orgId);
            if (!ObjectUtils.isEmpty(defaultGroup)) {
                this.updateById(defaultGroup.setDefaultValue(Status.FALSE.getKey()));
            }
        }
        // 组名如果修改了，判断组名是否存在
        if (!Objects.equals(usergroup.getGroupName(), rq.getGroupName())
                && this.checkNameRepeat(orgId, rq.getGroupName())) {
            log.info("用户组名称已存在，不能新增默认用户组！类：{} 方法：{}", "AuthUsergroupServiceImpl", "updateUsergroup");
            return ResponseResult.buildResponseResult(ResCodeEnum.NAME_EXIST);
        }
        org.springframework.beans.BeanUtils.copyProperties(rq, usergroup);
        if (!this.updateById(usergroup)) {
            log.error("用户组更新失败！类：{} 方法：{}", "AuthUsergroupServiceImpl", "updateUsergroup");
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<List<AuthUsergroupListDTO>> getUsergroupList(Integer orgId, String queryName, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        // 查询公司所有用户组
        Wrapper<AuthUsergroup> wrapper = new EntityWrapper<>(new AuthUsergroup()
                .setEnterpriseId(orgId)
                .setDeleted(Status.FALSE.getKey()));
        if (!StringUtils.isBlank(queryName)) {
            wrapper.like("group_name", queryName);
        }
        List<AuthUsergroup> groupList = usergroupMapper.selectPage(pagination, wrapper);
        if (CollectionUtils.isEmpty(groupList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0), PageUtils.transToPage(pagination));
        }
        // 查询用户组下所有 和用户的关联关系
        List<Integer> groupIds = groupList.stream().map(a -> a.getId()).collect(Collectors.toList());
        List<AuthUsergroupUser> usergroupUserList = usergroupUserService.selectList(new EntityWrapper<AuthUsergroupUser>()
                .in("usergroup_id", groupIds)
                .eq("deleted", Status.FALSE.getKey()));
        // 按组存储用户个数
        Map<Integer, Integer> countMap = Maps.newHashMap();
        for (AuthUsergroupUser o : usergroupUserList) {
            Integer usergroupId = o.getUsergroupId();
            Integer count = countMap.get(usergroupId);
            if (count == null) {
                countMap.put(usergroupId, 1);
            } else {
                countMap.put(usergroupId, ++count);
            }
        }
        // 返回结果
        List<AuthUsergroupListDTO> resultList = Lists.newArrayList();
        for (AuthUsergroup usergroup : groupList) {
            AuthUsergroupListDTO dto = BeanUtils.copyProperties(usergroup, AuthUsergroupListDTO.class);
            Integer userCount = countMap.get(dto.getId());
            dto.setUserCount(userCount == null ? 0 : userCount);
            resultList.add(dto);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resultList, PageUtils.transToPage(pagination));
    }

    @Override
    public ResponseResult<AuthUsergroupDetailDTO> getUsergroupDetail(Integer id) {
        AuthUsergroup usergroup = usergroupMapper.selectOne(new AuthUsergroup()
                .setId(id)
                .setDeleted(Status.FALSE.getKey()));
        if (ObjectUtils.isEmpty(usergroup)) {
            log.info("用户组不存在！类：{} 方法：{}", "AuthUsergroupServiceImpl", "getUsergroupDetail");
            return ResponseResult.buildResponseResult(ResCodeEnum.USERGROUP_NOT_EXIST);
        }
        AuthUsergroupDetailDTO dto = BeanUtils.copyProperties(usergroup, AuthUsergroupDetailDTO.class);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    @Override
    public List<AuthUsergroup> getByEnterpriseAndUserId(Integer orgId, Integer userId) {
        List<AuthUsergroupUser> groupUserList = usergroupUserService.selectList(new EntityWrapper<>(new AuthUsergroupUser()
                .setUserId(userId)
                .setDeleted(Status.FALSE.getKey())));
        // 查询默认用户组
        AuthUsergroup defaultGroup = this.selectOne(new EntityWrapper<>(new AuthUsergroup()
                .setEnterpriseId(orgId)
                .setDeleted(Status.FALSE.getKey())
                .setDefaultValue(Status.TRUE.getKey())));
        List<AuthUsergroup> resultList = null;
        // 如果没有用户组 查询默认用户组
        EntityWrapper<AuthUsergroup> wrapper = new EntityWrapper<>();
        if (!CollectionUtils.isEmpty(groupUserList)) {
            List<Integer> groupIds = groupUserList.stream().map(a -> a.getUsergroupId()).collect(Collectors.toList());
            if (!ObjectUtils.isEmpty(defaultGroup)) {
                Integer defaultGroupId = defaultGroup.getId();
                if (groupIds.contains(defaultGroupId)) {
                    // 如果用户关联的不仅仅只有默认组 则把默认组剔除
                    if (groupIds.size() > 1) {
                        groupIds = groupIds.stream().filter(a -> !a.equals(defaultGroupId)).collect(Collectors.toList());
                    }
                }
            }
            wrapper.setEntity(new AuthUsergroup()
                    .setDeleted(Status.FALSE.getKey())
                    .setEnterpriseId(orgId));
            wrapper.in("id", groupIds);
            resultList = this.selectList(wrapper);
            return resultList;
        }
        if (ObjectUtils.isEmpty(defaultGroup)) {
            return resultList;
        }
        resultList = new ArrayList<>(1);
        resultList.add(defaultGroup);
        return resultList;
    }

    @Override
    public ResponseResult<List<AuthUsergroupUsersDTO>> getGroupUsers(AuthUsergroupUsersListRQ rq, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        List<AuthUsergroupUsersDTO> groupUserList = usergroupMapper.getGroupUsers(pagination, rq);
        // 拼接用户部门层级关系
        Map<Integer, String> departmentMap = Maps.newHashMap();
        if (!CollectionUtils.isEmpty(groupUserList)) {
            Set<Integer> departmentIds = groupUserList.stream().map(a -> a.getDepartmentId()).collect(Collectors.toSet());
            if (!CollectionUtils.isEmpty(departmentIds)) {
                for (Integer departmentId : departmentIds) {
                    StringBuilder sb = new StringBuilder();
                    List<Departments> deparmentList = departmentsService.getParentAndSelf(departmentId, rq.getEnterpriseId());
                    if (!CollectionUtils.isEmpty(deparmentList)) {
                        int size = deparmentList.size();
                        int last = size - 1;
                        for (int i = 0; i < size; i++) {
                            sb.append(deparmentList.get(i).getName());
                            if (!Objects.equals(i, last)) {
                                sb.append("/");
                            }
                        }
                    }
                    departmentMap.put(departmentId, sb.toString());
                }
            }
        }
        groupUserList.forEach(a -> a.setDepartment(departmentMap.get(a.getDepartmentId())));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, groupUserList, PageUtils.transToPage(pagination));
    }

    @Override
    public ResponseResult<Boolean> checkHasDefault(int orgId) {
        AuthUsergroup usergroup = queryDefault(orgId);
        if (!ObjectUtils.isEmpty(usergroup)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, true);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, false);
    }
}

package com.bee.platform.user.authority.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.EnumRoleType;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.authority.dao.mapper.AuthUserRoleMapper;
import com.bee.platform.user.authority.dto.AuthRoleRoleTreeDTO;
import com.bee.platform.user.authority.dto.AuthUserRoleRoleIdsDTO;
import com.bee.platform.user.authority.dto.AuthUserRoleTreeDTO;
import com.bee.platform.user.authority.dto.UserInterfaceUriDTO;
import com.bee.platform.user.authority.entity.AuthRole;
import com.bee.platform.user.authority.entity.AuthUserRole;
import com.bee.platform.user.authority.rq.AuthUserRoleRQ;
import com.bee.platform.user.authority.rq.UserRelationRoleRQ;
import com.bee.platform.user.authority.service.AuthRoleService;
import com.bee.platform.user.authority.service.AuthUserRoleService;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 用户与角色（角色或功能的关联表）的中间表 服务实现类
 * </p>
 *
 * @author cheng.ke
 * @since 2019-05-20
 */
@Slf4j
@Service
public class AuthUserRoleServiceImpl extends ServiceImpl<AuthUserRoleMapper, AuthUserRole> implements AuthUserRoleService {

    @Autowired
    private AuthRoleService authRoleService;

    @Autowired
    AuthUserRoleMapper authUserRoleMapper;

    @Autowired
    private JedisService jedisService;


    @Transactional(rollbackFor = Exception.class)
    @Override
    public void assignPermissions(Integer userId,Integer enterpriseId, List<UserRelationRoleRQ> rq) {
        // 如果没有用户id 则不能进行操作
        if (ObjectUtils.isEmpty(userId)||ObjectUtils.isEmpty(enterpriseId)) {
            log.error("编辑用户权限失败，用户id为空");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ROLE_SAVE_FAILED_NO_USER_ID);
        }
        Date time = new Date();
        List<String> roleType = Lists.newArrayList("custom", "enterprise_admin", "super_admin");
        Wrapper<AuthUserRole> wrapper = new EntityWrapper<AuthUserRole>().eq("user_id", userId).eq("enterprise_id",enterpriseId).eq("deleted", 0).notIn("role_type", roleType);
        // 查询用户下是否有分配过的旧权限
        List<AuthUserRole> userRoleList = selectList(wrapper);
        // 有旧的权限 删除旧数据
        if (!CollectionUtils.isEmpty(userRoleList)) {
            AuthUserRole userRole = new AuthUserRole().setDeleted(1).setUpdateTime(time);
            if (!update(userRole, wrapper)) {
                log.error("给用户分配权限,删除旧数据失败，调用{}的{}方法出错", "AuthUserRoleServiceImpl", "assignPermissions()");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ROLE_SAVE_FAILED_DELETED_OLD_FAILED);
            }
        }
        // rq为空则不进行操作
        if (!CollectionUtils.isEmpty(rq)) {
            for (UserRelationRoleRQ ur : rq) {
                if (ObjectUtils.isEmpty(ur.getUserId()) || ObjectUtils.isEmpty(ur.getEnterpriseId())
                        || ObjectUtils.isEmpty(ur.getRoleId()) || ObjectUtils.isEmpty(ur.getPid()) || ObjectUtils.isEmpty(ur.getRoleType())|| ObjectUtils.isEmpty(ur.getFlag())
                ) {
                    log.error("给用户分配权限,请求参数不全-必须包含用户id、企业id、角色id、pid、roleType、flag ，调用{}的{}方法出错", "AuthUserRoleServiceImpl", "assignPermissions()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ROLE_SAVE_FAILED_ERROR_P);
                }
            }

            List<AuthUserRole> add = BeanUtils.assemble(AuthUserRole.class, rq);
            List<AuthUserRole> authUserRoles = add.stream().map(o -> o.setCreateTime(time)).collect(Collectors.toList());
            if (!insertBatch(authUserRoles)) {
                log.error("给用户分配权限保存失败，调用{}的{}方法出错", "AuthUserRoleServiceImpl", "assignPermissions()");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ROLE_SAVE_FAILED);
            }
        } else {

            log.info("rq为空，进行删除操作完成----用户id为：" + userId);
        }

        log.info("编辑用户权限完成---用户id为：" + userId);
    }


    @Override
    public List<UserRelationRoleRQ> getUserInCompanyRoleList(Integer userId, Integer enterpriseId) {
        List<String> roleType = Lists.newArrayList("custom", "enterprise_admin", "super_admin");

        List<AuthUserRole> authUserRoles = selectList(new EntityWrapper<AuthUserRole>().eq("deleted", 0)
                .eq("user_id", userId).eq("enterprise_id", enterpriseId).notIn("role_type",roleType).eq("flag",1));
        List<UserRelationRoleRQ> dto = BeanUtils.assemble(UserRelationRoleRQ.class, authUserRoles);


        return dto;
    }

    /**
     * 用户关联角色/功能/应用
     *
     * @param rq 请求参数
     * @return 操作结果
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void changeUserRole(AuthUserRoleRQ rq) {
        if (ObjectUtils.isEmpty(rq)) {
            return;
        }
        Date time = new Date();
        List<UserRelationRoleRQ> addList = rq.getAddList();
        List<UserRelationRoleRQ> deleteList = rq.getDeleteList();
        // 删除
        if (!CollectionUtils.isEmpty(deleteList)) {
            AuthUserRole userRole = new AuthUserRole().setUpdateTime(time).setDeleted(1);
            for (UserRelationRoleRQ deUr : deleteList) {
                if (ObjectUtils.isEmpty(deUr.getUserId()) || ObjectUtils.isEmpty(deUr.getEnterpriseId()) || ObjectUtils.isEmpty(deUr.getRoleId())) {
                    continue;
                }
                Wrapper<AuthUserRole> wrapper = new EntityWrapper<AuthUserRole>()
                        .eq("user_id", deUr.getUserId()).eq("enterprise_id", deUr.getEnterpriseId())
                        .eq("role_id", deUr.getRoleId()).eq("deleted", 0);
                List<AuthUserRole> have = selectList(wrapper);
                if (!CollectionUtils.isEmpty(have) && !update(userRole, wrapper)) {
                    log.error("添加用户与角色、功能、应用关联失败，调用{}的{}方法出错", "AuthUserRoleServiceImpl", "saveAllUserRole()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ROLE_SAVE_FAILED);
                }
            }

        }
        // 添加
        if (!CollectionUtils.isEmpty(addList)) {
            List<UserRelationRoleRQ> addRqList = Lists.newArrayList();
            for (UserRelationRoleRQ addUr : addList) {
                if (ObjectUtils.isEmpty(addUr.getUserId()) || ObjectUtils.isEmpty(addUr.getEnterpriseId())
                        || ObjectUtils.isEmpty(addUr.getRoleId()) || ObjectUtils.isEmpty(addUr.getPid())
                ) {
                    continue;
                }
                List<AuthUserRole> exist = selectList(new EntityWrapper<AuthUserRole>().eq("user_id", addUr.getUserId()).eq("enterprise_id", addUr.getEnterpriseId())
                        .eq("role_id", addUr.getRoleId()).eq("deleted", 0));
                if (CollectionUtils.isEmpty(exist)) {
                    addRqList.add(addUr);
                }
            }
            List<AuthUserRole> add = BeanUtils.assemble(AuthUserRole.class, addRqList);
            List<AuthUserRole> authUserRoles = add.stream().map(o -> o.setCreateTime(time)).collect(Collectors.toList());

            if (!CollectionUtils.isEmpty(authUserRoles) && !insertBatch(authUserRoles)) {
                log.error("添加用户与角色、功能、应用关联失败，调用{}的{}方法出错", "AuthUserRoleServiceImpl", "saveAllUserRole()");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ROLE_SAVE_FAILED);
            }
        }


    }

    private void saveAllUserRole(List<AuthUserRole> list) {
        if (CollectionUtils.isEmpty(list)) {
            log.error("添加用户与角色、功能、应用关联失败，调用{}的{}方法出错", "AuthUserRoleServiceImpl", "saveAllUserRole()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ROLE_SAVE_FAILED);
        }
        if (!insertBatch(list)) {
            log.error("添加用户与角色、功能、应用关联失败，调用{}的{}方法出错", "AuthUserRoleServiceImpl", "saveAllUserRole()");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ROLE_SAVE_FAILED);
        }

    }


    /**
     * 删除角色功能应用对应关系
     *
     * @param time   修改时间
     * @param userId 父级id
     */
    private void deleteUserRole(Date time, Integer userId) {
        if (!this.update(new AuthUserRole().setDeleted(1).setUpdateTime(time), new EntityWrapper<AuthUserRole>()
                .eq("user_id", userId).eq("deleted", 0))) {
            log.error("修改用户与角色、功能、应用失败，调用{}的{}方法出错", "AuthUserRoleServiceImpl", "deleteUserRole()");

            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.USER_ROLE_DELETE_FAILED);
        }
    }

    /**
     * 添加角色功能应用对应关系
     *
     * @param createId 创建人id
     * @param time     创建时间
     * @param childIds 子id集合
     * @param pId      父id
     */
    private void saveUserRole(Integer createId, Date time, List<Integer> childIds, Integer pId) {
        // 添加用户与角色/功能/应用关联关系
        for (Integer childId : childIds) {
            // 查询是否存在该角色/功能/应用
            AuthRole authRole = authRoleService.selectOne(new EntityWrapper<AuthRole>().eq("id", childId).eq("deleted", 0));
            // 查询是否关联过该角色/功能/应用
            AuthUserRole userRole = selectOne(new EntityWrapper<AuthUserRole>().eq("user_id", pId).eq("role_id", childId).eq("deleted", 0));
            if (!ObjectUtils.isEmpty(authRole) && ObjectUtils.isEmpty(userRole)) {
                AuthUserRole authUserRole = new AuthUserRole().setUserId(pId).setRoleId(childId).setCreateTime(time)
                        .setUpdateTime(time).setDeleted(0).setStatus(1).setCreateUser(createId);
                if (!insert(authUserRole)) {
                    log.error("添加用户与角色、功能、应用关联失败，调用{}的{}方法出错", "AuthUserRoleServiceImpl", "saveUserRole()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ROLE_SAVE_FAILED);
                }
            }
        }
    }


    /**
     * 查询用户角色树
     *
     * @param userId 用户id
     * @return 用户角色树
     */
    @Override
    public List<AuthUserRoleTreeDTO> getUserRoleTreeList(Integer userId) {
        List<AuthUserRoleTreeDTO> userRole = authUserRoleMapper.getUserRole(userId);
        if(CollectionUtils.isEmpty(userRole)){
            return new ArrayList<>();
        }
        List<AuthUserRoleTreeDTO> oneList = userRole.stream().filter(o -> o.getRoleType().equals(EnumRoleType.FUNCTION_ONE.getCode())).collect(Collectors.toList());
        List<Integer> appIds = oneList.stream().map(o -> o.getPid()).collect(Collectors.toList());
        if (CollectionUtils.isEmpty(appIds)) {
            return new ArrayList<>();
        }
        List<AuthRole> app = authRoleService.selectList(new EntityWrapper<AuthRole>().in("id", appIds));
        if (!CollectionUtils.isEmpty(oneList) && !CollectionUtils.isEmpty(app)) {
            for (AuthUserRoleTreeDTO o : oneList) {

                for (AuthRole authRole : app) {
                    if (o.getPid().equals(authRole.getId())) {
                        AuthUserRoleTreeDTO d = BeanUtils.copyProperties(authRole, AuthUserRoleTreeDTO.class);
                        d.setRoleId(authRole.getId()).setUserId(o.getUserId()).setEnterpriseId(o.getEnterpriseId()).setPid(0);
                        userRole.add(d);
                    }
                }
            }
        }

        Set<AuthUserRoleTreeDTO> collect = userRole.stream().filter(o -> !Arrays.asList("custom", "enterprise_admin", "super_admin").contains(o.getRoleType())).collect(Collectors.toSet());

        return build(collect);
    }

    /**
     * @param list :
     * @notes: 批量插入
     * @Author: junyang.li
     * @Date: 14:28 2019/5/24
     * @return: void
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void insertAll(List<AuthUserRole> list) {
        if(CollectionUtils.isEmpty(list)){
            return;
        }
        //每次插入1000条
        int start=0;
        int end=1000;
        int size=list.size();
        log.info("集合的容量是：{}",size);
        while (end<size){
            List<AuthUserRole> sub=list.subList(start,end);
            log.info("插入{}到{}条数据",start,end);
            authUserRoleMapper.insertAll(sub);
            start=end;
            end+=1000;
        }
        log.info("插入{}到{}条数据",start,size);
        List<AuthUserRole> sub=list.subList(start,size);
        authUserRoleMapper.insertAll(sub);
        log.info("数据插入完成.............");
    }

    /**
     * 两层循环实现建树
     *
     * @param treeNodes 传入的树节点列表
     * @return
     */
    public static List<AuthUserRoleTreeDTO> build(Set<AuthUserRoleTreeDTO> treeNodes) {

        List<AuthUserRoleTreeDTO> trees = new ArrayList<>();


        for (AuthUserRoleTreeDTO treeNode : treeNodes) {


            if (!ObjectUtils.isEmpty(treeNode.getPid()) && 0 == (treeNode.getPid())) {
                trees.add(treeNode);
            }

            for (AuthUserRoleTreeDTO it : treeNodes) {
                if (treeNode.getRoleId().equals(it.getPid())) {
                    if (treeNode.getChildren() == null) {
                        treeNode.setChildren(new ArrayList<>());
                    }
                    treeNode.getChildren().add(it);
                }
            }
        }
        return trees;
    }


    @Override
    public ResponseResult getUserRoleIds(Integer userId) {

        List<Integer> enterpriseIds = authUserRoleMapper.getUserEnterpriseIds(userId);
        Map<Integer, AuthUserRoleRoleIdsDTO> map = new HashMap<>(16);
        for (Integer enterpriseId : enterpriseIds) {
            AuthUserRoleRoleIdsDTO dto1 = new AuthUserRoleRoleIdsDTO();
            List<Integer> roleIds = authUserRoleMapper.getRoleIds(userId, enterpriseId);
            dto1.setEnterpriseId(enterpriseId);
            dto1.setRoleIds(roleIds);
            map.put(enterpriseId, dto1);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, map);
    }


    @Override
    public List<UserInterfaceUriDTO> getUserInterfaceUri(Integer userId, String platform) {
        try {
            String key = ConstantsUtil.COMMON_AUTH_USER_URL + userId;

            List<UserInterfaceUriDTO> dto;
            // 先从缓存查
            dto = jedisService.getJsonArrayObject(key, UserInterfaceUriDTO.class);
            if (ObjectUtils.isEmpty(dto)) {
                // 从数据库查询
                dto = authUserRoleMapper.getUserInterfaceUri(userId, platform);
                jedisService.delKey(key);
                jedisService.setJsonObject(key, dto, 0);
            }
            return dto;
        } catch (Exception e) {
            log.error("缓存连接异常，查询的键是：{}，异常信息是：{}", ConstantsUtil.COMMON_AUTH_USER_URL + userId, e);
            return authUserRoleMapper.getUserInterfaceUri(userId, platform);
        }
    }


    @Override
    public List<AuthRoleRoleTreeDTO> getRoleRoleTree() {
        // 查询所有application
        List<AuthRole> authRoles = authRoleService.selectList(new EntityWrapper<AuthRole>()
                .eq("role_type", "application").eq("deleted", 0));
        // 获取所有application的id
        List<Integer> aids = authRoles.stream().map(AuthRole::getId).collect(Collectors.toList());
        List<AuthRoleRoleTreeDTO> a = BeanUtils.assemble(AuthRoleRoleTreeDTO.class, authRoles);
        // 获取所有application
        List<AuthRoleRoleTreeDTO> application = a.stream().map(o -> o.setPid(0)).collect(Collectors.toList());
        // 获取所有functionOne
//        List<AuthRoleRoleTreeDTO> functionOne = authUserRoleMapper.getChildRole(aids);
//
//        List<Integer> f1Ids = functionOne.stream().map(AuthRoleRoleTreeDTO::getId).collect(Collectors.toList());
//        List<AuthRoleRoleTreeDTO> functionTwo = authUserRoleMapper.getChildRole(f1Ids);
//        List<Integer> f2Ids = functionTwo.stream().map(AuthRoleRoleTreeDTO::getId).collect(Collectors.toList());
//        List<AuthRoleRoleTreeDTO> base = authUserRoleMapper.getChildRole(f2Ids);

        List<Integer> allIds = Lists.newArrayList();
        List<AuthRoleRoleTreeDTO> all = authUserRoleMapper.getChildRole(allIds);

        List<AuthRoleRoleTreeDTO> functionOne = all.stream().filter(o -> aids.contains(o.getPid())).collect(Collectors.toList());
        List<Integer> oneIds = functionOne.stream().map(AuthRoleRoleTreeDTO::getId).collect(Collectors.toList());
        List<AuthRoleRoleTreeDTO> functionTwo = all.stream().filter(o -> oneIds.contains(o.getPid())).collect(Collectors.toList());
        List<Integer> twoIds = functionTwo.stream().map(AuthRoleRoleTreeDTO::getId).collect(Collectors.toList());
        List<AuthRoleRoleTreeDTO> base = all.stream().filter(o -> twoIds.contains(o.getPid())).collect(Collectors.toList());

        List<AuthRoleRoleTreeDTO> total = Lists.newArrayList();
        if (!CollectionUtils.isEmpty(application)) {
            total.addAll(application);
        }
        if (!CollectionUtils.isEmpty(functionOne)) {
            total.addAll(functionOne);
        }
        if (!CollectionUtils.isEmpty(functionTwo)) {
            total.addAll(functionTwo);
        }
        if (!CollectionUtils.isEmpty(base)) {
            total.addAll(base);
        }

        return buildAllRoleTree(total);
    }


    public static List<AuthRoleRoleTreeDTO> buildAllRoleTree(List<AuthRoleRoleTreeDTO> treeNodes) {

        List<AuthRoleRoleTreeDTO> trees = new ArrayList<>();
        for (AuthRoleRoleTreeDTO treeNode : treeNodes) {
            if (!ObjectUtils.isEmpty(treeNode.getPid()) && 0 == (treeNode.getPid())) {
                trees.add(treeNode);
            }

            for (AuthRoleRoleTreeDTO it : treeNodes) {
                if (treeNode.getId().equals(it.getPid())) {
                    if (treeNode.getChildren() == null) {
                        treeNode.setChildren(new ArrayList<>());
                    }
                    treeNode.getChildren().add(it);
                }
            }
        }
        return trees;
    }

}

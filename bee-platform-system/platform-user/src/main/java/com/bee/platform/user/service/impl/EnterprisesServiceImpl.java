package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.dto.IndustryDTO;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.enums.RoleType;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.Validator;
import com.bee.platform.user.constants.enums.EnumEnterpriseCheck;
import com.bee.platform.user.constants.enums.EnumRole;
import com.bee.platform.user.dao.mapper.*;
import com.bee.platform.user.dto.*;
import com.bee.platform.user.entity.*;
import com.bee.platform.user.entity.EnterprisesUsers;
import com.bee.platform.user.enums.UserStatus;
import com.bee.platform.user.service.EnterprisesCheckService;
import com.bee.platform.user.service.EnterprisesService;
import com.bee.platform.user.service.IndustryService;
import com.bee.platform.user.vo.UserManagementVO;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;

import java.lang.ref.SoftReference;
import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 服务实现类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */

@Slf4j
@Service
public class EnterprisesServiceImpl extends ServiceImpl<EnterprisesMapper, Enterprises> implements EnterprisesService {
    @Autowired
    private URolesMapper uRolesMapper;
    @Autowired
    private EnterprisesUsersMapper enterprisesUsersMapper;
    @Autowired
    private UserMapper userMapper;
    @Autowired
    private EnterprisesMapper enterprisesMapper;
    @Autowired
    private UsersRolesMapper usersRolesMapper;
    @Autowired
    private DepartmentsMapper departmentsMapper;
    @Autowired
    private UsersDepartmentsMapper usersDepartmentsMapper;
    @Autowired
    private EnterprisesCheckService enterprisesCheckService;
    @Autowired
    private IndustryService industryService;
    @Override
    public ResponseResult<EnterpriseDetailDTO> getById(Integer id) {
        Enterprises enterprises = selectById(id);
        EnterpriseDetailDTO dto=BeanUtils.copyProperties(enterprises,EnterpriseDetailDTO.class);
        //查询公司所属行业
        if(!StringUtils.isEmpty(enterprises.getIndustry())){
            IndustryDTO industry=industryService.selectIndustry(enterprises.getIndustry());
            dto.setIndustry(industry);
        }
        return ResponseResult.success(dto);
    }

    @Override
    public List<Enterprises> listByNames(List<String> names) {
        List<Enterprises> enterprisesList = enterprisesMapper.selectList(new EntityWrapper<Enterprises>().in("name", names));
        return enterprisesList;
    }

    @Override
    public ResponseResult<List<Map>> assembleEnterpriseInfo(Integer userId) {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * 获取管理员信息
     *
     * @param orgId
     * @return
     */
    @Override
    public List<AdminDTO> getAdmins(Integer orgId) {
        List<AdminDTO> data = new ArrayList<>();
        List<UserRole> userRoleList = new ArrayList<>();
        try {
            //查询用户角色列表
            if (orgId == null) {
                userRoleList = usersRolesMapper.selectList(new EntityWrapper<UserRole>());
            } else {
                userRoleList = usersRolesMapper.selectList(new EntityWrapper<UserRole>().eq("org_id", orgId));
            }
            for (UserRole userRole : userRoleList) {
                if (EnumRole.RoleType.superAdmin.getKey().equals(userRole.getRoleId())) {
                    AdminDTO adminDTO = new AdminDTO()
                            .setType(uRolesMapper.selectOne(new Role().setId(userRole.getRoleId())).getName())
                            .setPhone(userMapper.selectOne(new User().setId(userRole.getUserId())).getPhone())
                            .setNickname(enterprisesUsersMapper.selectOne(new EnterprisesUsers()
                                    .setUserId(userRole.getUserId()).setEnterpriseId(orgId)).getNickname());
                    data.add(adminDTO);
                    break;
                }
            }
            //根据企业id查询用户信息
            List<User> userList = userMapper.selectUsersByEnterpriseId(orgId);

            for (User user : userList) {
                List<Role> roleList = uRolesMapper.selectRoleList(user.getId());
                List<Departments> departmentsList = departmentsMapper.selectDepartmentsByUserId(user.getId());
                List<DepartmentDTO> departments = BeanUtils.assemble(DepartmentDTO.class, departmentsList);
                for (Role uRole : roleList) {
                    if ("admin".equals(uRole.getName())) {
                        AdminDTO adminDTO = new AdminDTO()
                                .setId(user.getId())
                                .setType(uRole.getName())
                                .setDepartment(departments)
                                .setPhone(user.getPhone())
                                .setNickname(enterprisesUsersMapper.selectOne(new EnterprisesUsers()
                                        .setUserId(user.getId()).setEnterpriseId(orgId)).getNickname());
                        StringBuilder sb = new StringBuilder();
                        for (Departments department : departmentsList) {
                            sb.append(usersDepartmentsMapper.selectOne(new UsersDepartments().setUserId(user.getId()).setDepartmentId(department.getId())).getPost()).append(",");
                        }

                        if (sb.length() != Status.FALSE.getKey()) {
                            adminDTO.setPost(sb.substring(0, sb.length() - 1));
                        }
                        data.add(adminDTO);
                        break;
                    }
                }
            }
        } catch (Exception e) {
            log.error("获取企业管理员查询失败", e);
        }
        return data;
    }

    /**
     * 添加管理员
     *
     * @param orgId
     * @param id
     * @return
     */
    @Override
    public ResponseResult<String> addAdmins(Integer orgId, int id) {

        //企业是否存在
        if (enterprisesMapper.selectCount(new EntityWrapper<Enterprises>().eq("id", orgId)) <= 0) {
            throw new BusinessException(ResCodeEnum.ENTERPRISE_NOT_EXIST, ExceptionMessageEnum.ENTERPRISE_NOT_EXIST);
        }
        //用户是否存在
        if (userMapper.selectCount(new EntityWrapper<User>().eq("id", id)) <= Status.FALSE.getKey()) {
            throw new BusinessException(ResCodeEnum.USER_NOT_EXIST, ExceptionMessageEnum.USER_NOT_EXIST);
        }
        //用户是否在企业中
        if (enterprisesUsersMapper.selectCount(new EntityWrapper<EnterprisesUsers>().eq("id", id).and().eq("enterprise_id", orgId)) <= 0) {
            throw new BusinessException(ResCodeEnum.USER_NOT_IN_ENTERPRISE, ExceptionMessageEnum.USER_NOT_IN_ENTERPRISE);
        }
        UserRole userRole = usersRolesMapper.selectOne(new UserRole().setId(id).setOrgId(orgId));
        //用户是否已经是管理员
        if (EnumRole.RoleType.admin.getKey().equals(userRole.getRoleId()) || EnumRole.RoleType.superAdmin.getKey().equals(userRole.getRoleId())) {
            throw new BusinessException(ResCodeEnum.USER_ALREADY_ADMIN, ExceptionMessageEnum.USER_ALREADY_ADMIN);
        }
        //保存用户角色信息
        if (usersRolesMapper.updateById(userRole.setRoleId(EnumRole.RoleType.admin.getKey())) <= Status.FALSE.getKey()) {
            log.error("调用{}的{}方法出错，用户角色组织关系保存异常", "addAdmins", "save()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.USER_ROLR_SAVE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 批量添加管理员
     *
     * @param orgId
     * @param old_ids
     * @param ids
     * @return
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public ResponseResult<String> addAdminsByList(Integer orgId, int[] old_ids, int[] ids) {

        try {
            //企业是否存在
            if (enterprisesMapper.selectCount(new EntityWrapper<Enterprises>().eq("id", orgId)) <= Status.FALSE.getKey()) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
            }
            //选择需要删除和添加的用户
            List<Integer> adds = new ArrayList<>();
            List<Integer> deletes = new ArrayList<>();
            boolean isExist;
            //需要添加的用户
            List<UserRole> userRoleList = new ArrayList<>();
            for (int id : ids) {
                isExist = false;
                for (int old_id : old_ids) {
                    if (id == old_id) {
                        isExist = true;
                        break;
                    }
                }
                if (!isExist) {
                    adds.add(id);
                }
            }

            //需要删除的用户
            for (int old_id : old_ids) {
                isExist = false;
                for (int id : ids) {
                    if (old_id == id) {
                        isExist = true;
                        break;
                    }
                }
                if (!isExist) {
                    deletes.add(old_id);
                }
            }

            if (adds.size() != 0) {
                //添加:排除不符合条件的用户
                for (Integer id : adds) {
                    //用户是否存在
                    if (userMapper.selectCount(new EntityWrapper<User>().eq("id", id)) <= Status.FALSE.getKey()) {
                        continue;
                    }
                    //用户是否在企业中
                    if (enterprisesUsersMapper.selectCount(new EntityWrapper<EnterprisesUsers>().eq("id", id).and().eq("enterprise_id", orgId)) <= 0) {
                        continue;
                    }
                    UserRole userRole = usersRolesMapper.selectOne(new UserRole().setId(id).setOrgId(orgId));
                    //用户是否已经是管理员
                    if (userRole.getRoleId().equals(EnumRole.RoleType.admin.getKey()) || userRole.getRoleId().equals(EnumRole.RoleType.superAdmin.getKey())) {
                        continue;
                    }

                    userRole.setRoleId(EnumRole.RoleType.admin.getKey());
                    userRoleList.add(userRole);
                }
            }

            if (deletes.size() != Status.FALSE.getKey()) {
                //删除:排除不符合条件的用户
                for (Integer id : deletes) {
                    //用户是否存在
                    if (userMapper.selectCount(new EntityWrapper<User>().eq("id", id)) <= Status.FALSE.getKey()) {
                        continue;
                    }
                    //用户是否在企业中
                    if (enterprisesUsersMapper.selectCount(new EntityWrapper<EnterprisesUsers>().eq("id", id).and().eq("enterprise_id", orgId)) <= 0) {
                        continue;
                    }
                    UserRole userRole = usersRolesMapper.selectOne(new UserRole().setId(id).setOrgId(orgId));
                    //用户是否已经是管理员
                    if (EnumRole.RoleType.user.getKey().equals(userRole.getRoleId())) {
                        continue;
                    }

                    userRole.setRoleId(EnumRole.RoleType.user.getKey());
                    userRoleList.add(userRole);
                }
            }
            //批量保存用户角色信息
            usersRolesMapper.insertAdminsList(userRoleList);
        } catch (Exception e) {
            log.error("调用{}的保存方法出错，用户角色组织关系保存异常,异常信息是：{}", "addAdminsByList", e);
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_ROLR_SAVE_FAILED);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 删除管理员
     *
     * @param orgId
     * @param id
     * @return
     */
    @Override
    public ResponseResult<String> deleteAdmins(Integer orgId, int id) {
        //企业是否存在
        if (enterprisesMapper.selectCount(new EntityWrapper<Enterprises>().eq("id", orgId)) <= Status.FALSE.getKey()) {
            throw new BusinessException(ResCodeEnum.ENTERPRISE_NOT_EXIST, ExceptionMessageEnum.ENTERPRISE_NOT_EXIST);
        }
        //用户是否存在
        if (userMapper.selectCount(new EntityWrapper<User>().eq("id", id)) <= Status.FALSE.getKey()) {
            throw new BusinessException(ResCodeEnum.USER_NOT_EXIST, ExceptionMessageEnum.USER_NOT_EXIST);
        }
        //用户是否在企业中
        if (enterprisesUsersMapper.selectCount(new EntityWrapper<EnterprisesUsers>().eq("id", id).and().eq("enterprise_id", orgId)) <= 0) {
            throw new BusinessException(ResCodeEnum.USER_NOT_IN_ENTERPRISE, ExceptionMessageEnum.USER_NOT_IN_ENTERPRISE);
        }
        UserRole userRole = usersRolesMapper.selectOne(new UserRole().setId(id).setOrgId(orgId));
        //用户是否是管理员
        if (userRole.getRoleId().equals(EnumRole.RoleType.user.getKey())) {
            throw new BusinessException(ResCodeEnum.USER_NOT_ADMIN, ExceptionMessageEnum.USER_NOT_ADMIN);
        }

        //保存用户角色信息
        if (usersRolesMapper.updateById(userRole.setRoleId(EnumRole.RoleType.user.getKey())) <= Status.FALSE.getKey()) {
            log.error("调用{}的{}方法出错，用户角色组织关系保存异常", "deleteAdmins", "updateById()");
            throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.USER_ROLR_SAVE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 批量删除管理员
     *
     * @param orgId
     * @param ids
     * @return
     */
    @Override
    public ResponseResult<String> deleteAdminsByList(Integer orgId, int[] ids) {
        try {
            //企业是否存在
            if (enterprisesMapper.selectCount(new EntityWrapper<Enterprises>().eq("id", orgId)) <= Status.FALSE.getKey()) {
                throw new BusinessException(ResCodeEnum.ENTERPRISE_NOT_EXIST, ExceptionMessageEnum.ENTERPRISE_NOT_EXIST);
            }
            List<UserRole> userRoleList = new ArrayList<>();
            for (int id : ids) {
                //用户是否存在
                if (userMapper.selectCount(new EntityWrapper<User>().eq("id", id)) <= Status.FALSE.getKey()) {
                    continue;
                }
                //用户是否在企业中
                if (enterprisesUsersMapper.selectCount(new EntityWrapper<EnterprisesUsers>().eq("id", id).and().eq("enterprise_id", orgId)) <= 0) {
                    continue;
                }
                UserRole userRole = usersRolesMapper.selectOne(new UserRole().setId(id).setOrgId(orgId));
                //用户是否已经是管理员
                if (userRole.getRoleId().equals(EnumRole.RoleType.user.getKey())) {
                    continue;
                }

                userRole.setRoleId(EnumRole.RoleType.user.getKey());
                userRoleList.add(userRole);
            }
            if (userRoleList.size() != Status.FALSE.getKey()) {
                //批量删除（修改）用户角色信息
                if (usersRolesMapper.updateAdminsList(userRoleList) <= Status.FALSE.getKey()) {
                    log.error("调用{}的{}方法出错，用户角色组织关系删除异常", "deleteAdminsByList", "updateAdminsList()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.USER_ROLR_SAVE_FAILED);
                }
            } else {
                throw new BusinessException(ResCodeEnum.ERROR_NOT_FOUND, ExceptionMessageEnum.ERROR_DATA);
            }
        } catch (Exception e) {
            log.error("调用{}方法出错，用户角色组织关系删除异常", "deleteAdminsByList");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.USER_ROLR_SAVE_FAILED);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 修改企业头像
     *
     * @param orgId
     * @param head
     * @return
     */
    @Override
    public ResponseResult<String> modifyEnterpriseHead(Integer orgId, String head) {
        Enterprises enterprise = enterprisesMapper.selectOne(new Enterprises().setId(orgId));
        if (ObjectUtils.isEmpty(enterprise)) {
            throw new BusinessException(ResCodeEnum.ENTERPRISE_NOT_EXIST, ExceptionMessageEnum.ENTERPRISE_NOT_EXIST);
        }
        if (!StringUtils.isBlank(head)) {
            enterprise.setHead(head);
        }
        if (enterprisesMapper.updateById(enterprise) <= Status.FALSE.getKey()) {
            log.error("调用{}的{}方法出错，企业变更失败", "modifyEnterpriseHead", "save()");
            throw new BusinessException(ResCodeEnum.SAVE_FAIL, ExceptionMessageEnum.ENTERPRISE_HEAD_UPDATE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 转让超级管理员
     *
     * @param userInfo
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<String> transferAdmin(AuthPlatformUserInfo userInfo, String phone) {
        //企业是否存在
        if (enterprisesMapper.selectCount(new EntityWrapper<Enterprises>().eq("id", userInfo.getOrgId())) <= Status.FALSE.getKey()) {
            throw new BusinessException(ResCodeEnum.ENTERPRISE_NOT_EXIST, ExceptionMessageEnum.ENTERPRISE_NOT_EXIST);
        }
        //用户是否存在
        if (userMapper.selectCount(new EntityWrapper<User>().eq("id", userInfo.getId())) <= Status.FALSE.getKey()) {
            throw new BusinessException(ResCodeEnum.USER_NOT_EXIST, ExceptionMessageEnum.USER_NOT_EXIST);
        }

        User user = userMapper.selectOne(new User().setPhone(phone));
        //用户是否在企业中
        if (enterprisesUsersMapper.selectCount(new EntityWrapper<EnterprisesUsers>().eq("user_id", user.getId()).and().eq("enterprise_id", userInfo.getOrgId())) <= 0) {
            throw new BusinessException(ResCodeEnum.USER_NOT_IN_ENTERPRISE, ExceptionMessageEnum.USER_NOT_IN_ENTERPRISE);
        }
        try {
            //usersRolesMapper.updateRoleId(1, org_id, user.getId());
            //usersRolesMapper.updateRoleId(3, org_id, currentUser.getId());
            usersRolesMapper.update(new UserRole().setRoleId(EnumRole.RoleType.superAdmin.getKey()),
                    new EntityWrapper<UserRole>().eq("org_id", userInfo.getOrgId()).and().eq("user_id", user.getId()));
            usersRolesMapper.update(new UserRole().setRoleId(EnumRole.RoleType.user.getKey()),
                    new EntityWrapper<UserRole>().eq("org_id", userInfo.getOrgId()).and().eq("user_id", userInfo.getId()));
        } catch (Exception e) {
            log.error("调用{}的{}方法出错，超级管理员转让失败", "transferAdmin", "update()");
            e.printStackTrace();
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes 获取当前用户的企业信息
     * @Author junyang.li
     * @Date 10:57 2019/3/18
     **/
    @Override
    public ResponseResult<List<EnterpriseListDTO>> getAllEnterprise(AuthPlatformUserInfo userInfo) {
        //查询用户关联的企业
        List<EnterprisesUsers> enterprisesUsers=enterprisesUsersMapper.selectList(new EntityWrapper<EnterprisesUsers>()
                .where("user_id={0} and is_active={1}",userInfo.getId(),Status.TRUE.getKey()));
        if (CollectionUtils.isEmpty(enterprisesUsers)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>());
        }
        List<Integer> enterpriseIds = enterprisesUsers.stream().map(EnterprisesUsers::getEnterpriseId).collect(Collectors.toList());
        return searchEnterpriseUser(enterpriseIds);
    }

    /**
     * @notes: 根据企业id，查询企业基本情况，部门数， 成员数等
     * @Author: junyang.li
     * @Date: 17:02 2019/5/27
     * @param enterpriseIds :
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.user.dto.EnterpriseListDTO>>
     */
    private ResponseResult<List<EnterpriseListDTO>> searchEnterpriseUser(List<Integer> enterpriseIds){
        //查询企业
        List<Enterprises> enterprises = enterprisesMapper.selectList(new EntityWrapper<Enterprises>()
                .in("id", enterpriseIds));
        //企业部门统计
        List<EnterprisesCountDTO> countDeparts = departmentsMapper.countDepartment(enterpriseIds);
        Map<Integer, Integer> departmentCount = new SoftReference<>(new HashMap<Integer, Integer>(16)).get();
        if (!CollectionUtils.isEmpty(countDeparts)) {
            countDeparts.forEach(obj -> departmentCount.put(obj.getOrgId(), obj.getCount()));
        }
        //企业人员统计
        List<EnterprisesCountDTO> enterprisesCountUsers = enterprisesUsersMapper.countUserByOrgIds(enterpriseIds);
        Map<Integer, Integer> userCount = new SoftReference<>(new HashMap<Integer, Integer>(16)).get();
        if (!CollectionUtils.isEmpty(enterprisesCountUsers)) {
            enterprisesCountUsers.forEach(obj -> userCount.put(obj.getOrgId(), obj.getCount()));
        }
        //遍历企业信息，添加公司人数、部门数
        List<EnterpriseListDTO> list = enterprises.stream().map(obj -> {
            return BeanUtils.copyProperties(obj, EnterpriseListDTO.class)
                    .setDepartmentNum(departmentCount.get(obj.getId()))
                    .setUserNum(userCount.get(obj.getId()));
        }).collect(Collectors.toList());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
    }

    /**
     * @notes 查询用户所在企业的信息
     * @Author junyang.li
     * @Date 15:17 2019/3/18
     **/
    private ResponseResult<List<EnterpriseListDTO>> getUserEnterprise(AuthPlatformUserInfo userInfo, List<Integer> roleIds) {
        List<UserRole> userRoles = null;
        if (CollectionUtils.isEmpty(roleIds)) {
            userRoles = usersRolesMapper.selectList(new EntityWrapper<UserRole>()
                    .where("user_id={0}", userInfo.getId()));
        } else {
            userRoles = usersRolesMapper.selectList(new EntityWrapper<UserRole>()
                    .where("user_id={0}", userInfo.getId()).and().in("role_id", roleIds));
        }
        if (CollectionUtils.isEmpty(userRoles)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>());
        }
        List<Integer> enterpriseIds = userRoles.stream().map(UserRole::getOrgId).collect(Collectors.toList());
        return searchEnterpriseUser(enterpriseIds);
    }

    /**
     * @notes 获取用户作为管理员的企业列表
     * @Author junyang.li
     * @Date 15:02 2019/3/18
     **/
    @Override
    public ResponseResult<List<EnterpriseListDTO>> getAllEnterpriseByAdmin(AuthPlatformUserInfo userInfo) {
        List<Integer> roleIds = new ArrayList<>();
        roleIds.add(RoleType.SUPER.getCode());
        roleIds.add(RoleType.ADMIN.getCode());
        return getUserEnterprise(userInfo, roleIds);
    }

    /**
     * @notes 根据企业id查询企业下成员信息
     * @Author junyang.li
     * @Date 11:47 2019/3/20
     **/
    @Override
    public ResponseResult<List<UserManagementDTO>> listUsersByEnterpriseId(Integer orgId, UserManagementVO vo) {
        ManagerSearchParamDTO dto = new ManagerSearchParamDTO();
        if (Validator.isMobile(vo.getKeyWord())) {
            dto.setUsername(vo.getKeyWord());
        } else {
            dto.setNickname(vo.getKeyWord());
        }
        dto.setOrgId(orgId);
        Pagination pagination = PageUtils.transFromPage(vo.getPage());
        //查询企业中的成员id
        List<UserManagementDTO> users = enterprisesUsersMapper.selectUserIdsByOrgId(dto, pagination);
        if (CollectionUtils.isEmpty(users)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(), PageUtils.transToPage(pagination));
        }
        List<Integer> userIds = users.stream().map(obj -> {
            obj.setUserStatus(UserStatus.USEING.getCode()).setUserStatusDesc(UserStatus.USEING.getDesc());
            return obj.getUserId();
        }).collect(Collectors.toList());
        //不为空，则查询这些用户的部门信息
        List<UserDepartmentDTO> list = departmentsMapper.ManagerSearchUser(orgId, userIds);
        if (CollectionUtils.isEmpty(list)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, users, PageUtils.transToPage(pagination));
        }
        Map<Integer, UserDepartmentDTO> map = new SoftReference<>(new HashMap<Integer, UserDepartmentDTO>(16)).get();
        for (UserDepartmentDTO item : list) {
            map.put(item.getUserId(), item);
        }
        users.stream().forEach(obj -> {
            UserDepartmentDTO department = map.get(obj.getUserId());
            if (department != null) {
                obj.setDepartmentId(department.getDepartmentId()).setDepartment(department.getDepartment());
            }
        });
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, users, PageUtils.transToPage(pagination));
    }

    /**
     * @notes 获得用户所在企业的基本信息
     * @Author junyang.li
     * @Date 14:16 2019/3/27
     **/
    @Override
    public ResponseResult<List<EnterpriseBasicDTO>> listBasicInfo(int userId) {
        List<Integer> enterpriseIds = enterprisesUsersMapper.listOrgIdsByUserId(userId);
        if (CollectionUtils.isEmpty(enterpriseIds)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_JOIN_ENTERPRISE);
        }
        //查询这些企业的信息
        List<Enterprises> enterprises = enterprisesMapper.selectList(new EntityWrapper<Enterprises>().in("id", enterpriseIds));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, BeanUtils.assemble(EnterpriseBasicDTO.class, enterprises));
    }

    /**
     * @notes 查询是否是注册企业
     * @Author laixy
     * @Date 14:09 2019/4/29
     **/
    @Override
    public ResponseResult<List<EnterpriseInfoDTO>> listRegisteredEnterpriseInfo(String enterpriseName) {
        List<EnterpriseDetailDTO> enterpriseDetailList = enterprisesMapper.listByEnterpriseName(enterpriseName);
        if (CollectionUtils.isEmpty(enterpriseDetailList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        if (CollectionUtils.size(enterpriseDetailList) >= 2) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_REPEAT);
        }
        //查询返回注册企业的信息
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, BeanUtils.assemble(EnterpriseInfoDTO.class, enterpriseDetailList));
    }

    /**
     * @param name 公司名称
     * @return 是否可注册
     */
    @Override
    public ResponseResult enterpriseCheck(AuthPlatformUserInfo userInfo, String name) {
        // 校验参数是否正确
        if (StringUtils.isEmpty(name)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        String companyName = name.replaceAll("\\s*", "");
        List<Enterprises> exist = this.selectList(new EntityWrapper<Enterprises>().eq("name", companyName)
                .eq("status",EnumCommon.LogicStatus.NORMAL.getKey()));
        // 查询企业审核表该企业是否有已通过的记录
        List<Integer> passedType= Arrays.asList(1,3,4,5);
        List<EnterprisesCheck> passed = enterprisesCheckService.selectList(new EntityWrapper<EnterprisesCheck>()
                .in("type", passedType)
                .eq("status",EnumCommon.LogicStatus.NORMAL.getKey())
                .eq("name", name));
        // 校验企业是否已注册
        if (CollectionUtils.isNotEmpty(exist)||CollectionUtils.isNotEmpty(passed) ) {
            log.info("企业已注册");
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_EXIST, EnumEnterpriseCheck.checkCompany.EXIST.getKey());
        }
        List<EnterprisesCheck> inAudit = enterprisesCheckService.selectList(new EntityWrapper<EnterprisesCheck>()
                .eq("name", companyName)
                .eq("type", EnumEnterpriseCheck.CheckType.IN_AUDIT_REGISTER.getKey())
//                .eq("create_id",userInfo.getId())
                .eq("status",EnumCommon.LogicStatus.NORMAL.getKey()));
        // 校验企业是否在审核中
        if (!CollectionUtils.isEmpty(inAudit)) {
            log.info("企业正在审核中");
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_IN_AUDIT, EnumEnterpriseCheck.checkCompany.IN_AUDIT.getKey());
        }
        List<EnterprisesCheck> refused = enterprisesCheckService.selectList(new EntityWrapper<EnterprisesCheck>()
                .eq("name", companyName)
                .eq("type", EnumEnterpriseCheck.CheckType.REFUSED_REGISTER.getKey())
//                .eq("create_id",userInfo.getId())
                .eq("status",EnumCommon.LogicStatus.NORMAL.getKey()));
        if(refused.size()>1){
            log.info("企业正在审核中");
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_IN_AUDIT, EnumEnterpriseCheck.checkCompany.IN_AUDIT.getKey());

        }
        if(CollectionUtils.isNotEmpty(refused)&&!refused.get(0).getCreateId().equals(userInfo.getId())){
            log.info("企业正在审核中");
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_IN_AUDIT, EnumEnterpriseCheck.checkCompany.IN_AUDIT.getKey());

        }
        // 企业可注册
        log.info("企业可注册");
        return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_REGISTERED_IS_OK, EnumEnterpriseCheck.checkCompany.NON_EXIST.getKey());

    }


    /**
     * 模糊查询企业列表
     * @param name 公司名称
     * @return 公司列表
     */
    @Override
    public ResponseResult<List<EnterpriseSearchDTO>> searchEnterpriseList(String name, Page page) {

        String companyName = name.replaceAll("\\s*", "");
        Pagination pagination= PageUtils.transFromPage(page);
        List<Enterprises> list = baseMapper.selectPage(pagination, new EntityWrapper<Enterprises>().like("name", companyName)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        List<EnterpriseSearchDTO> dto = BeanUtils.assemble(EnterpriseSearchDTO.class, list);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto,PageUtils.transToPage(pagination));

    }
}

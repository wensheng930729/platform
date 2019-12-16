package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.bcrypt.BCryptPassword;
import com.bee.platform.business.dto.DepartmentDTO;
import com.bee.platform.business.dto.DepartmentListDTO;
import com.bee.platform.business.dto.DepartmentTreePostDTO;
import com.bee.platform.business.rq.DepartmentAddRQ;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.constants.enums.EnumEnterpriseLevel;
import com.bee.platform.common.constants.enums.EnumMiddleNoticeTitle;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.enums.MiddleNoticeContentTemplate;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.authority.dao.mapper.AuthEnterpriseMapper;
import com.bee.platform.user.authority.dao.mapper.AuthPlatformUserEnterpriseMapper;
import com.bee.platform.user.authority.dto.AuthUsergroupDeparmentTreeDTO;
import com.bee.platform.user.authority.dto.AuthUsergroupUserListDTO;
import com.bee.platform.user.authority.entity.AuthEnterprise;
import com.bee.platform.user.authority.entity.AuthPlatformUser;
import com.bee.platform.user.authority.entity.AuthPlatformUserEnterprise;
import com.bee.platform.user.authority.service.AuthEnterpriseService;
import com.bee.platform.user.authority.service.AuthPlatformUserEnterpriseService;
import com.bee.platform.user.authority.service.AuthPlatformUserService;
import com.bee.platform.user.constants.enums.EnumEnterpriseCheck;
import com.bee.platform.user.constants.enums.EnumEnterpriseUser;
import com.bee.platform.user.dao.mapper.*;
import com.bee.platform.user.dto.DepartmentTreeDTO;
import com.bee.platform.user.entity.*;
import com.bee.platform.user.entity.EnterprisesUsers;
import com.bee.platform.user.service.*;
import com.bee.platform.user.sms.SmsService;
import com.bee.platform.user.utils.PinyinUtil;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import javax.servlet.http.HttpServletRequest;
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
public class DepartmentsServiceImpl extends ServiceImpl<DepartmentsMapper, Departments> implements DepartmentsService {

    @Autowired
    private DepartmentsMapper departmentsMapper;
    @Autowired
    private AuthEnterpriseMapper enterprisesMapper;
    @Autowired
    private DepartmentsMapper departmentMapper;
    @Autowired
    private UsersDepartmentsMapper usersDepartmentsMapper;
    @Autowired
    private AuthEnterpriseService enterprisesService;
    @Autowired
    private UserMapper usersMapper;
    @Autowired
    private UsersService usersService;
    @Autowired
    private EnterprisesUsersMapper enterprisesUsersMapper;
    @Autowired
    private EnterprisesUsersService enterprisesUsersService;
    @Autowired
    private UsersRolesMapper usersRolesMapper;
    @Autowired
    private UsersRolesService usersRolesService;
    @Autowired
    private SmsService smsService;
    @Autowired
    private URolesService uRolesService;
    @Autowired
    private ZPostService postService;
    @Autowired
    private MiddleSystemNoticeService middleSystemNoticeService;
    @Autowired
    private ConfigService configService;
    @Autowired
    private AuthPlatformUserEnterpriseService authPlatformUserEnterpriseService;
    @Autowired
    private AuthPlatformUserEnterpriseMapper authPlatformUserEnterpriseMapper;
    @Autowired
    private AuthPlatformUserService userService;

    /**
     * @notes 通过用户id和企业id查询用户所在部门
     * @Author junyang.li
     * @Date 9:18 2019/3/6
     **/
    @Override
    public DepartmentInfo selectByUserIdAndOrgId(Integer userId, Integer orgId) {
        List<DepartmentInfo> departmentInfos = departmentsMapper.selectByUserIdAndOrgId(userId, orgId);
        if (CollectionUtils.isEmpty(departmentInfos)) {
            return new DepartmentInfo();
        }
        DepartmentInfo department = departmentInfos.get(0);
        //查询职位
        ZPost post = postService.selectById(department.getPostId());
        if (post != null) {
            department.setPostName(post.getName());
        }
        return department;
    }

    /**
     * 获取部门层级关系（上级不包括自身）
     *
     * @param departmentId
     * @return
     */
    public String getDepartmentLevel(Integer departmentId) {

        Departments department = departmentMapper.selectOne(new Departments().setId(departmentId));
        if (ObjectUtils.isEmpty(department)) {
            return "";
        }
        Integer treeId = department.getTreeId();
        String result;
        switch (department.getLevel()) {
            case 1:
                result = department.getName();
                break;
            case 2:
                //1级
                Departments firstDep = departmentMapper.selectOne(new Departments().setId(treeId));
                if (ObjectUtils.isEmpty(firstDep)) {
                    return "";
                }
                result = firstDep.getName();
                break;
            case 3:
                //2级
                Departments secondDep = departmentMapper.selectOne(new Departments().setId(treeId));
                if (ObjectUtils.isEmpty(secondDep)) {
                    return "";
                }
                Integer secondDepTreeId = secondDep.getTreeId();
                //1级
                Departments firstDepartment = departmentMapper.selectOne(new Departments().setId(secondDepTreeId));
                if (null == firstDepartment) {
                    return "";
                }
                result = new StringBuilder(firstDepartment.getName()).append(" / ").append(secondDep.getName()).toString();
                break;
            case 4:
                //3级
                Departments thirdDepartment = departmentMapper.selectOne(new Departments().setId(treeId));
                if (ObjectUtils.isEmpty(thirdDepartment)) {
                    return "";
                }
                Integer thirdDepTreeId = thirdDepartment.getTreeId();
                //2级
                Departments secondDepartment = departmentMapper.selectOne(new Departments().setId(thirdDepTreeId));
                //1级
                if (ObjectUtils.isEmpty(secondDepartment)) {
                    return "";
                }
                Integer firstDepTreeId = secondDepartment.getTreeId();
                Departments firstDepart = departmentMapper.selectOne(new Departments().setId(firstDepTreeId));
                if (null == firstDepart) {
                    return "";
                }
                result = new StringBuilder(firstDepart.getName())
                        .append(" / ").append(secondDepartment.getName())
                        .append(" / ").append(thirdDepartment.getName()).toString();
                break;
            case 5:
                //4级
                Departments four = departmentMapper.selectOne(new Departments().setId(treeId));
                if (ObjectUtils.isEmpty(four)) {
                    return "";
                }
                //3级
                Departments third = departmentMapper.selectOne(new Departments().setId(four.getTreeId()));
                if (ObjectUtils.isEmpty(third)) {
                    return "";
                }
                Integer thirdTreeId = third.getTreeId();
                //2级
                Departments second = departmentMapper.selectOne(new Departments().setId(thirdTreeId));
                //1级
                if (ObjectUtils.isEmpty(second)) {
                    return "";
                }
                Integer firstTreeId = second.getTreeId();
                Departments first = departmentMapper.selectOne(new Departments().setId(firstTreeId));
                if (null == first) {
                    return "";
                }
                result = new StringBuilder(first.getName())
                        .append(" / ").append(second.getName())
                        .append(" / ").append(third.getName())
                        .append(" / ").append(four.getName()).toString();
                break;
            default:
                return "";
        }
        return result;
    }

    /**
     * 获取部门层级关系（上级包括自身）
     *
     * @param departmentId
     * @return
     */
    public String getDepartmentLevelWithId(Integer departmentId) {

        Departments department = departmentMapper.selectOne(new Departments().setId(departmentId));
        if (ObjectUtils.isEmpty(department)) {
            throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIND_FAILED);
        }
        Integer treeId = department.getTreeId();
        String result;
        String departmentName = String.valueOf(department.getId());
        switch (department.getLevel()) {
            //当前为一级部门
            case 1:
                result = departmentName;
                break;
            //当前为二级部门
            case 2:
                //一级
                Departments firstDep = departmentMapper.selectOne(new Departments().setId(treeId));
                if (ObjectUtils.isEmpty(firstDep)) {
                    throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIRST_FIND_FAILED);
                }
                result = new StringBuilder(firstDep.getId().toString()).append("-").append(departmentName).toString();
                break;
            //当前为三级部门
            case 3:
                //二级
                Departments secondDep = departmentMapper.selectOne(new Departments().setId(treeId));
                if (ObjectUtils.isEmpty(secondDep)) {
                    throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIRST_FIND_FAILED);
                }
                Integer secondDepTreeId = secondDep.getTreeId();
                //一级
                Departments firstDepartment = departmentMapper.selectOne(new Departments().setId(secondDepTreeId));
                result = new StringBuilder(firstDepartment.getId().toString())
                        .append("-").append(secondDep.getId())
                        .append("-").append(departmentName).toString();
                break;
            case 4:
                //3级
                Departments thirdDepartment = departmentMapper.selectOne(new Departments().setId(treeId));
                if (ObjectUtils.isEmpty(thirdDepartment)) {
                    throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIRST_FIND_FAILED);
                }
                Integer thirdDepTreeId = thirdDepartment.getTreeId();
                //2级
                Departments secondDepartment = departmentMapper.selectOne(new Departments().setId(thirdDepTreeId));
                //1级
                if (ObjectUtils.isEmpty(secondDepartment)) {
                    throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIRST_FIND_FAILED);
                }
                Integer firstDepTreeId = secondDepartment.getTreeId();
                Departments firstDepart = departmentMapper.selectOne(new Departments().setId(firstDepTreeId));
                result = new StringBuilder(firstDepart.getId().toString())
                        .append("-").append(secondDepartment.getId())
                        .append("-").append(thirdDepartment.getId())
                        .append("-").append(departmentName).toString();
                break;
            case 5:
                //4级
                Departments four = departmentMapper.selectOne(new Departments().setId(treeId));
                if (ObjectUtils.isEmpty(four)) {
                    throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIRST_FIND_FAILED);
                }
                //3级
                Departments third = departmentMapper.selectOne(new Departments().setId(four.getTreeId()));
                if (ObjectUtils.isEmpty(third)) {
                    throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIRST_FIND_FAILED);
                }
                Integer thirdTreeId = third.getTreeId();
                //2级
                Departments second = departmentMapper.selectOne(new Departments().setId(thirdTreeId));
                //1级
                if (ObjectUtils.isEmpty(second)) {
                    throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIRST_FIND_FAILED);
                }
                Integer firstTreeId = second.getTreeId();
                Departments first = departmentMapper.selectOne(new Departments().setId(firstTreeId));
                result = new StringBuilder(first.getId().toString())
                        .append("-").append(second.getId())
                        .append("-").append(third.getId())
                        .append("-").append(four.getId())
                        .append("-").append(departmentName).toString();
                break;
            default:
                throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIND_FAILED);
        }
        return result;
    }


    public String getDepartmentLevelWithSelf(Integer departmentId) {

        Departments department = departmentMapper.selectOne(new Departments().setId(departmentId));
        if (ObjectUtils.isEmpty(department)) {
            throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIND_FAILED);
        }
        Integer treeId = department.getTreeId();
        String result;
        String departmentName = department.getName();
        switch (department.getLevel()) {
            //当前为一级部门
            case 1:
                result = departmentName;
                break;
            //当前为二级部门
            case 2:
                //一级
                Departments firstDep = departmentMapper.selectOne(new Departments().setId(treeId));
                if (ObjectUtils.isEmpty(firstDep)) {
                    throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIRST_FIND_FAILED);
                }
                result = new StringBuilder(firstDep.getName()).append(" / ").append(departmentName).toString();
                break;
            //当前为三级部门
            case 3:
                //二级
                Departments secondDep = departmentMapper.selectOne(new Departments().setId(treeId));
                if (ObjectUtils.isEmpty(secondDep)) {
                    throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIRST_FIND_FAILED);
                }
                Integer secondDepTreeId = secondDep.getTreeId();
                //一级
                Departments firstDepartment = departmentMapper.selectOne(new Departments().setId(secondDepTreeId));
                result = new StringBuilder(firstDepartment.getName())
                        .append(" / ").append(secondDep.getName())
                        .append(" / ").append(departmentName).toString();
                break;
            case 4:
                //3级
                Departments thirdDepartment = departmentMapper.selectOne(new Departments().setId(treeId));
                if (ObjectUtils.isEmpty(thirdDepartment)) {
                    throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIRST_FIND_FAILED);
                }
                Integer thirdDepTreeId = thirdDepartment.getTreeId();
                //2级
                Departments secondDepartment = departmentMapper.selectOne(new Departments().setId(thirdDepTreeId));
                //1级
                if (ObjectUtils.isEmpty(secondDepartment)) {
                    throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIRST_FIND_FAILED);
                }
                Integer firstDepTreeId = secondDepartment.getTreeId();
                Departments firstDepart = departmentMapper.selectOne(new Departments().setId(firstDepTreeId));
                result = new StringBuilder(firstDepart.getName())
                        .append(" / ").append(secondDepartment.getName())
                        .append(" / ").append(thirdDepartment.getName())
                        .append(" / ").append(departmentName).toString();
                break;
            case 5:
                //4级
                Departments four = departmentMapper.selectOne(new Departments().setId(treeId));
                if (ObjectUtils.isEmpty(four)) {
                    throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIRST_FIND_FAILED);
                }
                //3级
                Departments third = departmentMapper.selectOne(new Departments().setId(four.getTreeId()));
                if (ObjectUtils.isEmpty(third)) {
                    throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIRST_FIND_FAILED);
                }
                Integer thirdTreeId = third.getTreeId();
                //2级
                Departments second = departmentMapper.selectOne(new Departments().setId(thirdTreeId));
                //1级
                if (ObjectUtils.isEmpty(second)) {
                    throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIRST_FIND_FAILED);
                }
                Integer firstTreeId = second.getTreeId();
                Departments first = departmentMapper.selectOne(new Departments().setId(firstTreeId));
                result = new StringBuilder(first.getName())
                        .append(" / ").append(second.getName())
                        .append(" / ").append(third.getName())
                        .append(" / ").append(four.getName())
                        .append(" / ").append(departmentName).toString();
                break;
            default:
                throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIND_FAILED);
        }
        return result;
    }

    /**
     * 获取当前部门层级关系（树）
     *
     * @param level
     * @return
     */
    @Override
    public ResponseResult<List<DepartmentDTO>> getDepartmentTree(AuthPlatformUserInfo userInfo, Integer level) {

        Integer orgId = userInfo.getOrgId();
        List<Departments> departments = departmentMapper.selectList(new EntityWrapper<Departments>().eq("org_id", orgId));
        if (CollectionUtils.isEmpty(departments)) {
            throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIND_FAILED);
        }
        List<DepartmentDTO> result = new ArrayList<>();
        //根据选择层级 返回所有上级部门
        if (!ObjectUtils.isEmpty(level)) {
            switch (level) {
                //勾选1级
                case 1:
                    break;
                //勾选2级
                case 2:
                    //所有1级部门
                    List<Departments> departments2 = departmentMapper.selectList(new EntityWrapper<Departments>()
                            .eq("org_id", orgId).eq("level", EnumEnterpriseLevel.levelNode.first_level.getKey()));
                    if (!CollectionUtils.isEmpty(departments2)) {
                        List<DepartmentDTO> dto = BeanUtils.assemble(DepartmentDTO.class, departments2);
                        result = dto;
                    }
                    break;
                //3级
                case 3:
                    //所有1 2级
                    List<Departments> departments3 = departmentMapper.selectList(new EntityWrapper<Departments>()
                            .eq("org_id", orgId).eq("level", EnumEnterpriseLevel.levelNode.first_level.getKey())
                            .or()
                            .eq("org_id", orgId).eq("level", EnumEnterpriseLevel.levelNode.second_level.getKey()));
                    if (!CollectionUtils.isEmpty(departments3)) {
                        result = BeanUtils.assemble(DepartmentDTO.class, departments3);
                    }
                    break;
                //4级
                case 4:
                    //所有1 2 3级
                    List<Departments> departments4 = departmentMapper.selectList(new EntityWrapper<Departments>()
                            .eq("org_id", orgId).eq("level", EnumEnterpriseLevel.levelNode.first_level.getKey())
                            .or()
                            .eq("org_id", orgId).eq("level", EnumEnterpriseLevel.levelNode.second_level.getKey())
                            .or()
                            .eq("org_id", orgId).eq("level", EnumEnterpriseLevel.levelNode.third_level.getKey()));
                    if (!CollectionUtils.isEmpty(departments4)) {
                        result = BeanUtils.assemble(DepartmentDTO.class, departments4);
                    }
                    break;
                case 5:
                    //所有1 2 3 4级
                    List<Departments> departments5 = departmentMapper.selectList(new EntityWrapper<Departments>()
                            .eq("org_id", orgId).eq("level", EnumEnterpriseLevel.levelNode.first_level.getKey())
                            .or()
                            .eq("org_id", orgId).eq("level", EnumEnterpriseLevel.levelNode.second_level.getKey())
                            .or()
                            .eq("org_id", orgId).eq("level", EnumEnterpriseLevel.levelNode.third_level.getKey())
                            .or()
                            .eq("org_id", orgId).eq("level", EnumEnterpriseLevel.levelNode.fourth_level.getKey()));
                    if (!CollectionUtils.isEmpty(departments5)) {
                        result = BeanUtils.assemble(DepartmentDTO.class, departments5);
                    }
                    break;
                default:
                    throw new BusinessException(ResCodeEnum.DEPARTMENT_NOT_EXIST, ExceptionMessageEnum.DEPARTMENT_FIND_FAILED);
            }
        } else {
            List<Departments> departmentsAll = departmentMapper.selectList(new EntityWrapper<Departments>()
                    .eq("org_id", orgId));
            if (!CollectionUtils.isEmpty(departmentsAll)) {
                result = BeanUtils.assemble(DepartmentDTO.class, departmentsAll);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    @Override
    public ResponseResult<List<DepartmentTreePostDTO>> getDepartmentTreeForPost(AuthPlatformUserInfo userInfo) {

        Integer orgId = userInfo.getOrgId();
        Integer secondLevelKey = EnumEnterpriseLevel.levelNode.second_level.getKey();
        Integer thirdLevelKey = EnumEnterpriseLevel.levelNode.third_level.getKey();
        Integer fourthLevelKey = EnumEnterpriseLevel.levelNode.fourth_level.getKey();
        Integer fifthLevelKey = EnumEnterpriseLevel.levelNode.fifth_level.getKey();

        if (ObjectUtils.isEmpty(orgId)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NO_AUTHORITY);
        }
        List<Departments> departments1 = departmentMapper.selectList(new EntityWrapper<Departments>()
                .eq("org_id", orgId).eq("level", EnumEnterpriseLevel.levelNode.first_level.getKey()));
        List<DepartmentTreePostDTO> result = new ArrayList<>();
        //1级部门
        if (!CollectionUtils.isEmpty(departments1)) {
            for (Departments department1 : departments1) {
                //1级部门参数
                DepartmentTreePostDTO dto1 = BeanUtils.copyProperties(department1, DepartmentTreePostDTO.class);
                List<DepartmentTreePostDTO> sub1 = new ArrayList<>();
                //1级部门下2级部门
                List<Departments> departments2 = departmentMapper.selectList(new EntityWrapper<Departments>()
                        .eq("tree_id", department1.getId())
                        .eq("org_id", orgId)
                        .eq("level", secondLevelKey));
                for (Departments dep2 : departments2) {
                    //2级部门参数
                    Departments department2 = departmentMapper.selectOne(new Departments().setId(dep2.getId()).setOrgId(orgId).setLevel(secondLevelKey));
                    if (ObjectUtils.isEmpty(department2)) {
                        continue;
                    }
                    DepartmentTreePostDTO dto2 = BeanUtils.copyProperties(department2, DepartmentTreePostDTO.class);
                    List<DepartmentTreePostDTO> sub2 = new ArrayList<>();
                    //2级部门下3级部门
                    List<Departments> departments3 = departmentMapper.selectList(new EntityWrapper<Departments>()
                            .eq("tree_id", dep2.getId())
                            .eq("org_id", orgId)
                            .eq("level", thirdLevelKey));
                    for (Departments department3 : departments3) {
                        //3级部门参数
                        DepartmentTreePostDTO dto3 = BeanUtils.copyProperties(department3, DepartmentTreePostDTO.class);
                        List<DepartmentTreePostDTO> sub3 = new ArrayList<>();
                        //3级下4级部门
                        List<Departments> departments4 = departmentMapper.selectList(new EntityWrapper<Departments>()
                                .eq("tree_id", department3.getId())
                                .eq("org_id", orgId)
                                .eq("level", fourthLevelKey));
                        for (Departments deps4 : departments4) {
                            //4级部门参数
                            Departments department4 = departmentMapper.selectOne(new Departments().setId(deps4.getId()).setOrgId(orgId).setLevel(fourthLevelKey));
                            if (ObjectUtils.isEmpty(department4)) {
                                continue;
                            }
                            DepartmentTreePostDTO dto4 = BeanUtils.copyProperties(department4, DepartmentTreePostDTO.class);
                            //5级部门
                            List<Departments> departments5 = departmentMapper.selectList(new EntityWrapper<Departments>()
                                    .eq("tree_id", deps4.getId())
                                    .eq("org_id", orgId)
                                    .eq("level", fifthLevelKey));
                            List<DepartmentTreePostDTO> dep5 = BeanUtils.assemble(DepartmentTreePostDTO.class, departments5);
                            sub3.add(dto4);
                            dto4.setSubList(dep5);
                        }
                        sub2.add(dto3);
                        dto3.setSubList(sub3);
                    }
                    sub1.add(dto2);
                    dto2.setSubList(sub2);
                }
                result.add(dto1);
                dto1.setSubList(sub1);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }


    @Override
    public ResponseResult<List<DepartmentListDTO>> getDepartmentList(AuthPlatformUserInfo userInfo, String name, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        //当前用户部门及下级部门集合
        Integer orgId = userInfo.getOrgId();
        Wrapper<Departments> condition = new EntityWrapper<Departments>().eq("org_id", orgId);
        if (!StringUtils.isEmpty(name)) {
            condition.like("name", name);
        }
        condition.orderBy("update_at", false);

        // 获取公司下所有部门
        List<Departments> allDepartments = departmentMapper.selectList(new EntityWrapper<Departments>().eq("org_id", orgId));

        List<Departments> departments = departmentMapper.selectPage(pagination, condition);
        List<DepartmentListDTO> result = new ArrayList<>();
        if (!CollectionUtils.isEmpty(departments)) {
            for (Departments dep : departments) {
                DepartmentListDTO dto = BeanUtils.copyProperties(dep, DepartmentListDTO.class);
                Integer actualId = dep.getId();
                //部门下人数
                ArrayList<Integer> resultList = new ArrayList<>();
                resultList.add(actualId);
                iterDescendants(resultList, dep.getId(), allDepartments);
                Integer count = authPlatformUserEnterpriseService.selectCount(new EntityWrapper<AuthPlatformUserEnterprise>()
                        .eq("enterprise_id", orgId)
                        .in("departments_id", resultList)
                        .ne("departments_id", 0)
                        .eq("status", Status.TRUE.getKey())
                        .eq("deleted", Status.FALSE.getKey()));
                dto.setCount(count);
                //隶属关系
                dto.setRelation(this.getDepartmentLevel(actualId));
                result.add(dto);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result, PageUtils.transToPage(pagination));
    }

    @Override
    public ResponseResult<Integer> departmentNameCheck(AuthPlatformUserInfo userInfo, String name,
                                                       Integer level, Integer departmentId, Integer treeId) {
        Integer orgId = userInfo.getOrgId();
        Wrapper wrapper = new EntityWrapper<Departments>()
                .eq("org_id", orgId)
                .eq("level", level)
                .eq("name", name);
        if (!Objects.isNull(treeId)) {
            wrapper.eq("tree_id", treeId);
        }
        List<Departments> departments = departmentsMapper.selectList(wrapper);
        if (CollectionUtils.isEmpty(departments)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, EnumEnterpriseCheck.IS_EXISTED.NO.getKey());
        }
        if (Objects.nonNull(departmentId)) {
            for (Departments d : departments) {
                if (!d.getId().equals(departmentId)) {
                    return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, EnumEnterpriseCheck.IS_EXISTED.YES.getKey());
                }
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, EnumEnterpriseCheck.IS_EXISTED.NO.getKey());
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, EnumEnterpriseCheck.IS_EXISTED.YES.getKey());
    }

    /**
     * 新增部门
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<?> add(AuthPlatformUserInfo userInfo, DepartmentAddRQ rq) {
        Integer orgId = userInfo.getOrgId();
        String name = rq.getName();
        Integer pid = rq.getTreeId();
        Integer id = rq.getDepartmentId();
        if (checkDuplicateDepartmentName(orgId, pid, id, name)) {
            Departments departmentNew = new Departments();
            departmentNew.setOrgId(orgId);
            departmentNew.setTreeId(rq.getTreeId());
            departmentNew.setLevel(rq.getLevel());
            departmentNew.setName(name);
            departmentNew.setDescription(rq.getDescription());
            departmentNew.setCreateAt(new Date());
            departmentNew.setUpdateAt(new Date());
            if (departmentMapper.insert(departmentNew) <= 0) {
                throw new BusinessException(ResCodeEnum.DEPARTMENT_SAVE_FAILED, ExceptionMessageEnum.DEPARTMENT_SAVE_FAILED);
            } else {
                // 若为1级部门，则更新treeId为部门id
                if (ObjectUtils.isEmpty(pid)) {
                    departmentNew.setTreeId(departmentNew.getId());
                    if (departmentMapper.updateById(departmentNew) <= 0) {
                        throw new BusinessException(ResCodeEnum.DEPARTMENT_SAVE_FAILED, ExceptionMessageEnum.DEPARTMENT_SAVE_FAILED);
                    }
                }
            }
        } else {
            throw new BusinessException(ResCodeEnum.ERROR_SYSTEM, ExceptionMessageEnum.DEPARTMENT_NAME_EXISTED);
        }
        //中台系统通知
        String title = EnumMiddleNoticeTitle.title.ADD_DEPARTMENT_NOTICE.getValue();
        String orgName = userInfo.getOrg_name();
        middleSystemNoticeService.createNotice(userInfo.getId(), title, MiddleNoticeContentTemplate.ENTERPRISE_ADMIN_ADD_DEPARTMENT.getKey(), orgName, name);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 编辑部门
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<?> editDepartment(AuthPlatformUserInfo userInfo, DepartmentAddRQ rq) {
        String name = rq.getName();
        Integer id = rq.getDepartmentId();
        Integer orgId = userInfo.getOrgId();
        Departments old = selectById(id);
        if (ObjectUtils.isEmpty(old)) {
            log.info("部门[id={}]已被删除", id);
            throw new BusinessException(ResCodeEnum.DEPARTMENT_SAVE_FAILED, ExceptionMessageEnum.DEPARTMENT_FIND_FAILED);
        }
        Integer pid = old.getTreeId();
        if (pid == null) {
            log.info("数据异常, 部门[id= {}]treeId为null", id);
            throw new BusinessException(ResCodeEnum.DEPARTMENT_SAVE_FAILED, ExceptionMessageEnum.DEPARTMENT_SAVE_FAILED);
        }
        if (checkDuplicateDepartmentName(orgId, pid, id, name)) {
            old.setName(name);
            old.setDescription(rq.getDescription());
            old.setUpdateAt(new Date());
            if (departmentMapper.updateById(old) <= 0) {
                throw new BusinessException(ResCodeEnum.DEPARTMENT_SAVE_FAILED, ExceptionMessageEnum.DEPARTMENT_SAVE_FAILED);
            }
        } else {
            throw new BusinessException(ResCodeEnum.ERROR_SYSTEM, ExceptionMessageEnum.DEPARTMENT_NAME_EXISTED);
        }
        //中台系统通知
        String title = EnumMiddleNoticeTitle.title.EDIT_DEPARTMENT_NOTICE.getValue();
        String orgName = userInfo.getOrg_name();
        middleSystemNoticeService.createNotice(userInfo.getId(), title, MiddleNoticeContentTemplate.ENTERPRISE_ADMIN_EDIT_DEPARTMENT.getKey(), orgName, name);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult deleteDepartment(AuthPlatformUserInfo userInfo, Integer department_id) {
        String orgName = userInfo.getOrg_name();

        //验证下属部门是否为空
        List<Departments> departmentAll = departmentMapper.selectList(new EntityWrapper<Departments>().eq("org_id", userInfo.getOrgId()));
        if (CollectionUtils.isEmpty(departmentAll)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        // 获取当前部门、以及当前部门下的子部门的id
        List<Integer> resultList = new ArrayList<>();
        resultList.add(department_id);
        iterDescendants(resultList, department_id, departmentAll);
        //删除部门
        if (!CollectionUtils.isEmpty(resultList)) {
            if (departmentMapper.deleteBatchIds(resultList) <= 0) {
                log.info("删除部门表departments表的={}失败，数据表中可能无数据", resultList);
            }
            //验证部门信息
            int count = authPlatformUserEnterpriseService.selectCount(new EntityWrapper<AuthPlatformUserEnterprise>().eq("departments_id", department_id).eq("deleted", EnumCommon.IsDeleted.not_Delete.getKey()));
            if (count == 0) {
                log.info("验证部门：department_id={}, count={}", department_id, count);
            }
            // 获取部门下所有职位
            List<AuthPlatformUserEnterprise> platformUserEnterprises = authPlatformUserEnterpriseService.selectList(new EntityWrapper<AuthPlatformUserEnterprise>()
                    .in("departments_id", resultList));
            List<Integer> postIds = Lists.newArrayList();
            if (!CollectionUtils.isEmpty(platformUserEnterprises)) {
                postIds = platformUserEnterprises.stream().map(a -> a.getPostId()).collect(Collectors.toList());
            }
            if (authPlatformUserEnterpriseService.update(new AuthPlatformUserEnterprise()
                            .setDepartmentsId(0)
                            .setUpdateTime(new Date()),
                    new EntityWrapper<AuthPlatformUserEnterprise>().in("departments_id", resultList))) {
                log.error("删除用户、企业、部门、职位关联表auth_platform_user_enterprise的={}失败，数据表中可能无数据", resultList);
            }
            // 删除职位
            if (!CollectionUtils.isEmpty(postIds)) {
                if (!postService.deleteBatchIds(postIds)) {
                    log.error("删除职位表z_post的={}失败，数据表中可能无数据", postIds);
                    return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
                }
            }

        }
        //中台系统通知
//        String title = EnumMiddleNoticeTitle.title.DELETE_DEPARTMENT_NOTICE.getValue();
//        middleSystemNoticeService.createNotice(userInfo.getId(), title, MiddleNoticeContentTemplate.ENTERPRISE_ADMIN_DELETE_DEPARTMENT.getKey(), orgName, department.getName());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult addUserForDepartment(int org_id, int[] dep_ids, String nickname, String phone, String post) {
        //验证企业
        AuthEnterprise enterprise = enterprisesService.selectOne(new EntityWrapper<AuthEnterprise>().eq("id", org_id));
        if (ObjectUtils.isEmpty(enterprise)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        List<Departments> departments = new ArrayList<>();
        //获取企业默认部门
        List<Departments> eDepartments = departmentMapper.selectList(new EntityWrapper<Departments>()
                .eq("name", enterprise.getName())
                .orderBy("create_at", true));
        if (!CollectionUtils.isEmpty(eDepartments)) {
            Departments eDepartment = eDepartments.get(0);
            departments.add(eDepartment);
        }
        //phone获取用户
        User user = usersService.selectOne(new EntityWrapper<User>().eq("phone", phone));
        //非平台用户
        if (ObjectUtils.isEmpty(user)) {
            System.err.println("添加的用户不是平台用户...");
            user = new User();
            user.setUuid(UUID.randomUUID().toString().replaceAll("-", ""));
            user.setPhone(phone);
            user.setUsername(phone);
            user.setNickname(phone);
            //初始密码
            user.setPassword(BCryptPassword.encode("123456"));
            user.setCreateAt(new Date());
            user.setUpdateAt(new Date());
            //用户新增
            if (usersMapper.insert(user) <= 0) {
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_SAVE_FAILED);
            }
            //用户id
            Integer userId = user.getId();
            //用户企业关系新增
            EnterprisesUsers eu = new EnterprisesUsers();
            eu.setUserId(userId);
            eu.setEnterpriseId(enterprise.getId());
            eu.setNickname(nickname);
            eu.setNicknamePinyin(PinyinUtil.toPinyin(nickname));
            eu.setPost(post);
            eu.setIsActive(EnumCommon.IsActive.not_active.getKey());
            eu.setIsInvite(EnumCommon.IsActive.not_active.getKey());
            if (enterprisesUsersMapper.insert(eu) <= 0) {
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ENTERPRISE_RELATION_SAVE_FAILED);
            }
            //用户部门关系新增
            for (Departments dp : departments) {
                UsersDepartments ud = new UsersDepartments();
                ud.setPost(post);
                ud.setUserId(userId);
                ud.setDepartmentId(dp.getId());
                if (usersDepartmentsMapper.insert(ud) <= 0) {
                    throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_DEPARTMENT_RELATION_SAVE_FAILED);
                }
            }
            //用户角色关系新增
            Role role = uRolesService.selectOne(new EntityWrapper<Role>().eq("name", "user"));
            UserRole ur = new UserRole();
            ur.setUserId(userId);
            ur.setRoleId(role.getId());
            ur.setOrgId(org_id);
            if (usersRolesMapper.insert(ur) <= 0) {
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ROLE_RELATION_SAVE_FAILED);
            }
            //同步用户到im
            //accountService.synToIm(phone);
        } else {
            //获取用户相关部门信息
            Integer userId = user.getId();
            List<UsersDepartments> usersDepartments = usersDepartmentsMapper.selectList(new EntityWrapper<UsersDepartments>().eq("user_id", userId));
            List<Departments> list = new ArrayList<>();
            if (!CollectionUtils.isEmpty(usersDepartments)) {
                for (UsersDepartments ud : usersDepartments) {
                    list.add(departmentMapper.selectOne(new Departments().setId(ud.getDepartmentId())));
                }
                if (list.size() != 0) {
                    for (int i = 0; i < list.size(); i++) {
                        if (list.get(i).getOrgId().equals(org_id)) {
                            for (int j = 0; j < departments.size(); j++) {
                                if (list.get(i).getId().equals(departments.get(j).getId())) {
                                    return ResponseResult.buildResponseResult(ResCodeEnum.USER_AT_ALREADY);
                                }
                            }
                        }
                    }
                }
            }
            UserRole userRole = usersRolesService.selectOne(new EntityWrapper<UserRole>().eq("user_id", user.getId()).eq("org_id", org_id));
            if (!ObjectUtils.isEmpty(userRole)) {
                System.err.println("添加的用户是平台用户,并且已经属于该企业...");
                for (Departments department : departments) {
                    UsersDepartments departmentRelation = new UsersDepartments();
                    departmentRelation.setDepartmentId(department.getId());
                    departmentRelation.setUserId(user.getId());
                    if (usersDepartmentsMapper.insert(departmentRelation) <= 0) {
                        log.error("调用{}的{}方法出错，用户部门关系保存异常", "addUserForDepartment", "save()");
                        throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_DEPARTMENT_RELATION_SAVE_FAILED);
                    }
                    //企业用户信息保存
                    EnterprisesUsers userEnterpriseRelation = enterprisesUsersService.selectOne(
                            new EntityWrapper<EnterprisesUsers>().eq("user_id", user.getId()).eq("enterprise_id", org_id));
                    if (ObjectUtils.isEmpty(userEnterpriseRelation)) {
                        return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED);
                    }
                    userEnterpriseRelation.setPost(post);
                    if (enterprisesUsersMapper.updateById(userEnterpriseRelation) <= 0) {
                        throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ENTERPRISE_RELATION_SAVE_FAILED);
                    }
                }
            } else {
                System.err.println("添加的用户是平台用户,不属于该企业...");
                UserRole userRoleOrgRelation = new UserRole();
                userRoleOrgRelation.setOrgId(org_id);
                userRoleOrgRelation.setUserId(user.getId());
                userRoleOrgRelation.setRoleId(3);
                if (usersRolesMapper.insert(userRoleOrgRelation) <= 0) {
                    log.error("调用{}的{}方法出错，用户角色组织关系保存异常", "addUserForDepartment", "save()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ROLE_RELATION_SAVE_FAILED);
                }
                //企业用户关系新增
                EnterprisesUsers enterpriseRelation = new EnterprisesUsers();
                enterpriseRelation.setUserId(user.getId());
                enterpriseRelation.setEnterpriseId(org_id);
                enterpriseRelation.setNickname(nickname);
                enterpriseRelation.setPost(post);
                if (enterprisesUsersMapper.insert(enterpriseRelation) <= 0) {
                    log.error("调用{}的{}方法出错，用户企业关系保存异常", "addUserForDepartment", "save()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ENTERPRISE_RELATION_SAVE_FAILED);
                }
                //部门用户关系新增
                for (Departments department : departments) {
                    UsersDepartments departmentRelation = new UsersDepartments();
                    departmentRelation.setUserId(user.getId());
                    departmentRelation.setDepartmentId(department.getId());
                    if (usersDepartmentsMapper.insert(departmentRelation) <= 0) {
                        log.error("调用{}的{}方法出错，用户部门关系保存异常", "addUserForDepartment", "save()");
                        throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_DEPARTMENT_RELATION_SAVE_FAILED);
                    }
                }
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult addUserToMultiDep(int org_id, int[] dep_ids, String nickname, String phone, String post) {
        //验证企业
        AuthEnterprise enterprise = enterprisesService.selectOne(new EntityWrapper<AuthEnterprise>().eq("id", org_id));
        if (ObjectUtils.isEmpty(enterprise)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        //所有待添加用户部门
        List<Departments> departments = new ArrayList<>();
        for (Integer dep_id : dep_ids) {
            Departments department = departmentMapper.selectOne(new Departments().setId(dep_id));
            if (ObjectUtils.isEmpty(department)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.DEPARTMENT_NOT_EXIST);
            }
            departments.add(department);
        }
        //phone获取用户
        User user = usersService.selectOne(new EntityWrapper<User>().eq("phone", phone));
        if (ObjectUtils.isEmpty(user)) {
            log.error("添加的用户不是平台用户...");
            System.err.println("添加的用户不是平台用户...");
            String defaultPassword = configService.getConfValue(ConstantsUtil.DEFAULT_PASSWORD, "123456", "系统配置的默认密码");
            user = new User();
            user.setUuid(UUID.randomUUID().toString().replaceAll("-", ""));
            user.setPhone(phone);
            user.setUsername(phone);
            user.setNickname(phone);
            //初始密码
            user.setPassword(BCryptPassword.encode(defaultPassword));
            user.setCreateAt(new Date());
            user.setUpdateAt(new Date());
            //用户新增
            if (usersMapper.insert(user) <= 0) {
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_SAVE_FAILED);
            }
            //用户id
            Integer userId = user.getId();
            //用户企业关系新增
            EnterprisesUsers eu = new EnterprisesUsers();
            eu.setUserId(userId);
            eu.setEnterpriseId(enterprise.getId());
            eu.setNickname(nickname);
            eu.setNicknamePinyin(PinyinUtil.toPinyin(nickname));
            eu.setPost(post);
            eu.setIsActive(EnumCommon.IsActive.not_active.getKey());
            eu.setIsInvite(EnumCommon.IsActive.not_active.getKey());
            if (enterprisesUsersMapper.insert(eu) <= 0) {
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ENTERPRISE_RELATION_SAVE_FAILED);
            }
            //用户部门关系新增
            for (Departments dp : departments) {
                UsersDepartments ud = new UsersDepartments();
                ud.setUserId(userId);
                ud.setDepartmentId(dp.getId());
                ud.setPost(post);
                if (usersDepartmentsMapper.insert(ud) <= 0) {
                    throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_DEPARTMENT_RELATION_SAVE_FAILED);
                }
            }
            //用户角色关系新增
            Role role = uRolesService.selectOne(new EntityWrapper<Role>().eq("name", "user"));
            UserRole ur = new UserRole();
            ur.setUserId(userId);
            ur.setRoleId(role.getId());
            ur.setOrgId(org_id);
            if (usersRolesMapper.insert(ur) <= 0) {
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ROLE_RELATION_SAVE_FAILED);
            }
            //同步用户到im
            //accountService.synToIm(phone);
        } else {
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_ALREADY_EXIST);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<Map<String, Integer>> getDepartmentCount(int org_id) {
        //公司信息
        AuthEnterprise enterprises = enterprisesMapper.selectById(org_id);
        if (ObjectUtils.isEmpty(enterprises)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        HashMap<String, Integer> map = new HashMap<>(2);
        //组织数
        Integer count = departmentMapper.selectCount(new EntityWrapper<Departments>().eq("org_id", org_id));
        map.put("count", count);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, map);
    }

    @Override
    public ResponseResult<List<Departments>> getDepartmentForOrg(AuthPlatformUserInfo userInfo, int org_id, int dep_id) {
        if (org_id == 0) {
            org_id = userInfo.getOrgId();
        }
        AuthEnterprise enterprises = enterprisesMapper.selectById(org_id);
        if (ObjectUtils.isEmpty(enterprises)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        //组织id treeId获取部门信息
        List<Departments> result = departmentMapper.selectList(
                new EntityWrapper<Departments>().eq("org_id", org_id).eq("tree_id", dep_id));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    @Override
    public ResponseResult<List<HashMap<String, Object>>> getOneStepDepartmentForOrg(AuthPlatformUserInfo userInfo, int org_id) {
        if (org_id == 0) {
            org_id = userInfo.getOrgId();
        }
        //企业信息
        AuthEnterprise enterprises = enterprisesMapper.selectById(org_id);
        if (ObjectUtils.isEmpty(enterprises)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        List<HashMap<String, Object>> result = new ArrayList<>();
        List<Departments> departments = departmentMapper.selectList(
                new EntityWrapper<Departments>().eq("org_id", org_id));
        if (CollectionUtils.isEmpty(departments)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.DEPARTMENT_NOT_EXIST);
        }
        //一级部门treeId 获取所有一级部门
        int tree_id = departments.get(0).getId();
        departments.clear();
        departments = departmentMapper.selectList(
                new EntityWrapper<Departments>().eq("org_id", org_id).eq("tree_id", tree_id));
        if (CollectionUtils.isEmpty(departments)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.DEPARTMENT_NOT_EXIST);
        }
        for (int i = 0; i < departments.size(); i++) {
            //参数组装
            Departments department = departments.get(i);
            HashMap<String, Object> map = new HashMap<>();
            map.put("id", department.getId());
            map.put("tree_id", department.getTreeId());
            map.put("org_id", org_id);
            map.put("name", department.getName());
            map.put("createAt", department.getCreateAt());
            map.put("updateAt", department.getUpdateAt());
            result.add(i, map);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    @Override
    public ResponseResult<List<Object>> getOneStepDepartmentFromOrgForIM(AuthPlatformUserInfo userInfo) {
        //用户获取相关企业id
        List<EnterprisesUsers> enterprisesUsers = enterprisesUsersMapper.selectList(
                new EntityWrapper<EnterprisesUsers>().eq("user_id", userInfo.getId()));
        if (CollectionUtils.isEmpty(enterprisesUsers)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED);
        }
        List<Integer> enterpriseIds = enterprisesUsers.stream().map(e -> e.getEnterpriseId()).collect(Collectors.toList());
        List<Object> result = new ArrayList<>();
        //根企业
        if (enterpriseIds.size() == 1) {
            result.add(getOneStepDepartmentForOrg(userInfo, 0).getObject());
        } else {
            //获取一级企业信息
            for (Integer enterpriseId : enterpriseIds) {
                result.add(getOneStepDepartmentForOrg(userInfo, enterpriseId).getObject());
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    @Override
    public ResponseResult<Map<String, Object>> getUserListForDepartment(AuthPlatformUserInfo userInfo, int org_id, int id, Page page) {
        if (org_id == 0) {
            org_id = userInfo.getOrgId();
        }
        //企业信息
        AuthEnterprise enterprise = enterprisesMapper.selectById(org_id);
        if (ObjectUtils.isEmpty(enterprise)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        //部门信息
        Departments department = departmentMapper.selectById(id);
        if (ObjectUtils.isEmpty(department)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.DEPARTMENT_NOT_EXIST);
        }
        Map<String, Object> jsonData = new LinkedHashMap<>();
        List<HashMap<String, Object>> data = new ArrayList<>();
        //分页查询部门用户信息
        Pagination pagination = PageUtils.transFromPage(page);
        List<UsersDepartments> relations = usersDepartmentsMapper.selectPage(
                pagination, new EntityWrapper<UsersDepartments>().eq("department_id", id));
        if (CollectionUtils.isEmpty(relations)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.DEPARTMENT_NOT_EXIST);
        }
        for (UsersDepartments relation : relations) {
            //用户 用户与部门相关信息
            User user = usersMapper.selectById(relation.getUserId());
            HashMap<String, Object> json = new HashMap<>();
            EnterprisesUsers userEnterpriseRelation = enterprisesUsersService.selectOne(
                    new EntityWrapper<EnterprisesUsers>().eq("user_id", user.getId()).eq("enterprise_id", org_id));
            if (userEnterpriseRelation != null) {
                json.put("org_nickname", userEnterpriseRelation.getNickname());
                json.put("is_invite", userEnterpriseRelation.getIsInvite());
                json.put("is_active", userEnterpriseRelation.getIsActive());
                json.put("post", userEnterpriseRelation.getPost());
            }
            json.put("id", user.getId());
            json.put("phone", user.getPhone());
            json.put("identifier", user.getIdentifier());
            json.put("sig", user.getSig());
            json.put("active", user.getIsActive() + "");
            json.put("head", user.getHead());
            json.put("nickname", user.getNickname());
            data.add(json);
        }
        //部门id 获取人数
        Integer userCount = usersDepartmentsMapper.selectCount(new EntityWrapper<UsersDepartments>().eq("department_id", id));
        long countTemp = userCount / page.getPageSize();
        long countTemp2 = userCount % page.getPageSize();
        long count;
        if (countTemp == 0) {
            count = 1;
        } else if (countTemp2 == 0) {
            count = countTemp;
        } else {
            count = countTemp + 1;
        }
        jsonData.put("count", userCount);
        jsonData.put("pages", count);
        jsonData.put("data", data);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, jsonData);
    }

    @Override
    public ResponseResult<Map<String, Object>> getUserInfoForOrg(AuthPlatformUserInfo userInfo, int org_id, int id) {
        if (org_id == 0) {
            org_id = userInfo.getOrgId();
        }
        //企业信息
        AuthEnterprise enterprise = enterprisesMapper.selectById(org_id);
        if (ObjectUtils.isEmpty(enterprise)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }

        //用户 用户与部门相关信息
        User user = usersMapper.selectById(id);
        HashMap<String, Object> json = new HashMap<>();
        EnterprisesUsers userEnterpriseRelation = enterprisesUsersService.selectOne(
                new EntityWrapper<EnterprisesUsers>().eq("user_id", user.getId()).eq("enterprise_id", org_id));
        if (userEnterpriseRelation != null) {
            json.put("org_nickname", userEnterpriseRelation.getNickname());
            json.put("post", userEnterpriseRelation.getPost());
            json.put("is_invite", userEnterpriseRelation.getIsInvite());
            json.put("is_active", userEnterpriseRelation.getIsActive());
        }
        json.put("id", user.getId());
        json.put("phone", user.getPhone());
        json.put("identifier", user.getIdentifier());
        json.put("sig", user.getSig());
        json.put("active", user.getIsActive() + "");
        json.put("head", user.getHead());
        json.put("nickname", user.getNickname());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, json);
    }

    @Override
    public ResponseResult<Map<String, Object>> getAllUserForEnterprise(AuthPlatformUserInfo userInfo, Page page) {
        int org_id = userInfo.getOrgId();
        //企业信息
        AuthEnterprise enterprises = enterprisesMapper.selectById(org_id);
        if (ObjectUtils.isEmpty(enterprises)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        Map<String, Object> jsonData = new LinkedHashMap<>();
        List<HashMap<String, Object>> data = new ArrayList<>();
        //企业用户信息
        Pagination pagination = PageUtils.transFromPage(page);
        List<EnterprisesUsers> userEnterpriseRelations = enterprisesUsersMapper.selectPage(pagination,
                new EntityWrapper<EnterprisesUsers>().eq("enterprise_id", org_id));
        if (CollectionUtils.isEmpty(userEnterpriseRelations)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED);
        }
        for (EnterprisesUsers userEnterpriseRelation : userEnterpriseRelations) {
            //参数组装
            HashMap<String, Object> json = new HashMap<>();
            json.put("org_nickname", userEnterpriseRelation.getNickname());
            int user_id = userEnterpriseRelation.getUserId();
            json.put("id", user_id);
            json.put("phone", usersMapper.selectById(org_id).getPhone());
            json.put("is_invite", userEnterpriseRelation.getIsInvite());
            json.put("is_active", userEnterpriseRelation.getIsActive());
            json.put("post", userEnterpriseRelation.getPost());
            data.add(json);
        }
        //公司人数
        Integer userCount = enterprisesUsersMapper.selectCount(new EntityWrapper<EnterprisesUsers>().eq("enterprise_id", org_id));
        long countTemp = userCount / page.getPageSize();
        long countTemp2 = userCount % page.getPageSize();
        long count;
        if (countTemp == 0) {
            count = 1;
        } else if (countTemp2 == 0) {
            count = countTemp;
        } else {
            count = countTemp + 1;
        }
        jsonData.put("count", userCount);
        jsonData.put("pages", count);
        jsonData.put("data", data);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, jsonData);
    }

    @Override
    public ResponseResult<List<HashMap<String, Object>>> getActivedUser(AuthPlatformUserInfo userInfo) {
        int org_id = userInfo.getOrgId();
        AuthEnterprise enterprises = enterprisesMapper.selectById(org_id);
        if (ObjectUtils.isEmpty(enterprises)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        List<HashMap<String, Object>> data = new ArrayList<>();
        //企业用户信息
        List<EnterprisesUsers> userEnterpriseRelations = enterprisesUsersMapper.selectList(
                new EntityWrapper<EnterprisesUsers>().eq("enterprise_id", org_id));
        if (CollectionUtils.isEmpty(userEnterpriseRelations)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED);
        }
        for (EnterprisesUsers userEnterpriseRelation : userEnterpriseRelations) {
            UserRole userRole = usersRolesService.selectOne(
                    new EntityWrapper<UserRole>().eq("user_id", userEnterpriseRelation.getUserId()).eq("org_id", org_id));
            if (userEnterpriseRelation.getIsActive() == 0 || userRole.getRoleId() == 1) {
                continue;
            }
            HashMap<String, Object> json = new HashMap<>();
            json.put("nickname", userEnterpriseRelation.getNickname());
            int user_id = userEnterpriseRelation.getUserId();
            json.put("id", user_id);
            json.put("phone", usersMapper.selectById(org_id).getPhone());
            json.put("is_invite", userEnterpriseRelation.getIsInvite());
            json.put("is_active", userEnterpriseRelation.getIsActive());
            data.add(json);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, data);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult changeDepartmentName(int id, String name) {
        Departments department = departmentMapper.selectById(id);
        if (ObjectUtils.isEmpty(department)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.DEPARTMENT_NOT_EXIST);
        }
        department.setName(name);
        if (departmentMapper.updateById(department) <= 0) {
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.DEPARTMENT_SAVE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<HashMap<String, Object>> getDepartmentInfo(int org_id, int id) {
        HashMap<String, Object> json = new HashMap<>();
        //部门人数
        Integer count = usersDepartmentsMapper.selectCount(new EntityWrapper<UsersDepartments>().eq("department_id", id));
        json.put("userNum", count);
        //参数组装
        Departments department = departmentMapper.selectById(id);
        if (ObjectUtils.isEmpty(department)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.DEPARTMENT_NOT_EXIST);
        }
        json.put("name", department.getName());
        json.put("id", department.getId());
        json.put("orgName", enterprisesMapper.selectById(org_id).getName());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, json);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult inviteUser(HttpServletRequest request, int user_id) {
        ResponseResult message = new ResponseResult();
        //用户信息
        UserInfo userInfo = usersService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        int org_id = userInfo.getOrgId();
        AuthEnterprise enterprise = enterprisesMapper.selectById(org_id);
        if (ObjectUtils.isEmpty(enterprise)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        EnterprisesUsers userEnterpriseRelation = enterprisesUsersService.selectOne(
                new EntityWrapper<EnterprisesUsers>().eq("user_id", user_id).eq("enterprise_id", org_id));
        //新用户
        if (ObjectUtils.isEmpty(userEnterpriseRelation)) {
            String org_name = enterprisesMapper.selectById(org_id).getName();
            User user = usersMapper.selectById(user_id);
            try {
                message = smsService.sendMessageForInvit(user.getPhone(), org_name);
            } catch (Exception e) {
                e.printStackTrace();
                message.setCode(0);
                message.setMessage(ResCodeEnum.BACKSTAGE_EXCEPTION.getMsg());
                ;
            }
            //新增用户与企业关系信息
            userEnterpriseRelation = new EnterprisesUsers().setUserId(user_id)
                    .setEnterpriseId(org_id)
                    .setIsInvite(EnumEnterpriseUser.InviteType.is_invite.getKey())
                    .setIsActive(EnumEnterpriseUser.ActiveType.not_active.getKey())
                    .setNickname(user.getNickname())
                    .setNicknamePinyin(user.getNicknamePinyin());
            enterprisesUsersService.insert(userEnterpriseRelation);
            message.setMessage(ExceptionMessageEnum.SEND_INVITE_SUCCESS.getMessage());
            return message;
        }
        //未邀请用户
        if (userEnterpriseRelation.getIsInvite() == 0) {
            String org_name = enterprisesMapper.selectById(org_id).getName();
            User user = usersMapper.selectById(user_id);
            try {
                message = smsService.sendMessageForInvit(user.getPhone(), org_name);
            } catch (Exception e) {
                e.printStackTrace();
                message.setCode(0);
                message.setMessage(ResCodeEnum.SEND_MESSAGE_FAIL.getMsg());
            }
            //发送成功 更新邀请状态
            if (message.getCode() == 1) {
                EnterprisesUsers enterprisesUser = enterprisesUsersService.selectOne(
                        new EntityWrapper<EnterprisesUsers>().eq("user_id", user_id).eq("enterprise_id", org_id));
                if (ObjectUtils.isEmpty(enterprisesUser)) {
                    return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED);
                }
                enterprisesUser.setIsInvite(EnumEnterpriseUser.InviteType.is_invite.getKey());
                enterprisesUser.setIsActive(EnumEnterpriseUser.ActiveType.not_active.getKey());
                if (enterprisesUsersMapper.updateById(enterprisesUser) <= 0) {
                    throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISE_SAVE_FAILED);
                }
                message.setMessage(ExceptionMessageEnum.SEND_INVITE_SUCCESS.getMessage());
            } else {
                message.setMessage(ExceptionMessageEnum.SEND_INVITE_FAILED.getMessage());
            }
        } else {
            message.setCode(0);
            message.setMessage(ExceptionMessageEnum.SEND_INVITE_ALREADY.getMessage());
        }
        return message;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult inviteUser(HttpServletRequest request, int[] user_ids) {
        ResponseResult message = new ResponseResult();
        //用户信息
        UserInfo userInfo = usersService.getUserInfo(request.getHeader(ConstantsUtil.SYS_TOKEN));
        if (ObjectUtils.isEmpty(userInfo)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        int org_id = userInfo.getOrgId();
        AuthEnterprise enterprises = enterprisesMapper.selectById(org_id);
        if (ObjectUtils.isEmpty(enterprises)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }

        String org_name = enterprises.getName();
        String phone = "";
        for (int i = 0; i < user_ids.length; i++) {
            User user = usersMapper.selectById(user_ids[i]);
            EnterprisesUsers userEnterpriseRelation = enterprisesUsersService.selectOne(
                    new EntityWrapper<EnterprisesUsers>().eq("user_id", user.getId()).eq("enterprise_id", org_id));
            if (userEnterpriseRelation.getIsInvite() == 0) {
                if (i == user_ids.length - 1) {
                    phone += user.getPhone();
                    break;
                }
                phone += user.getPhone() + ",";
            }
        }
        try {
            message = smsService.sendMessageForInvit(phone, org_name);
        } catch (Exception e) {
            e.printStackTrace();
            message.setCode(0);
            message.setMessage(ResCodeEnum.SEND_MESSAGE_FAIL.getMsg());
        }
        if (message.getCode() == 1) {
            for (Integer user_id : user_ids) {
                EnterprisesUsers enterprisesUser = enterprisesUsersService.selectOne(
                        new EntityWrapper<EnterprisesUsers>().eq("user_id", user_id).eq("enterprise_id", org_id));
                if (ObjectUtils.isEmpty(enterprisesUser)) {
                    return ResponseResult.buildResponseResult(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED);
                }
                enterprisesUser.setIsInvite(EnumEnterpriseUser.InviteType.is_invite.getKey());
                enterprisesUser.setIsActive(EnumEnterpriseUser.ActiveType.not_active.getKey());
                if (enterprisesUsersMapper.updateById(enterprisesUser) <= 0) {
                    throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISE_SAVE_FAILED);
                }
            }
            message.setMessage(ExceptionMessageEnum.SEND_INVITE_SUCCESS.getMessage());
        } else {
            message.setMessage(ExceptionMessageEnum.SEND_INVITE_FAILED.getMessage());
        }
        return message;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult removeUser(AuthPlatformUserInfo userInfo, int dep_id, int user_id) {
        if (ObjectUtils.isEmpty(userInfo.getOrgId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_USER_ROLE);
        }
        int org_id = userInfo.getOrgId();
        AuthEnterprise enterprises = enterprisesMapper.selectById(org_id);
        if (ObjectUtils.isEmpty(enterprises)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        if (usersDepartmentsMapper.delete(
                new EntityWrapper<UsersDepartments>().eq("department_id", dep_id).eq("user_id", user_id)) <= 0) {
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISE_DELETE_USER_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult modifyUserInfo(int org_id, int id, int[] dep_ids, String nickname, String phone, String post) {
        AuthEnterprise enterprise = enterprisesMapper.selectById(org_id);
        if (ObjectUtils.isEmpty(enterprise)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        //修改手机号
        User user = usersService.selectOne(new EntityWrapper<User>().eq("phone", phone));
        if (ObjectUtils.isEmpty(user)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        if (!StringUtils.isEmpty(user.getPhone()) && !StringUtils.isEmpty(phone) && !user.getPhone().equals(phone)) {
            user.setPhone(phone);
            if (usersMapper.updateById(user) <= 0) {
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_SAVE_FAILED);
            }
        }
        EnterprisesUsers userEnterpriseRelation = enterprisesUsersService.selectOne(
                new EntityWrapper<EnterprisesUsers>().eq("user_id", id).eq("enterprise_id", org_id));
        //修改企业昵称
        if (!StringUtils.isEmpty(userEnterpriseRelation.getNickname()) && !StringUtils.isEmpty(nickname) && !nickname.equals(userEnterpriseRelation.getNickname())) {
            userEnterpriseRelation.setNickname(nickname);
            userEnterpriseRelation.setNicknamePinyin(PinyinUtil.toPinyin(nickname));
            if (enterprisesUsersMapper.updateById(userEnterpriseRelation) <= 0) {
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISE_SAVE_FAILED);
            }
        }
        //修改职务
        if (!StringUtils.isEmpty(userEnterpriseRelation.getPost()) && !StringUtils.isEmpty(post) && !userEnterpriseRelation.getPost().equals(post)) {
            userEnterpriseRelation.setPost(post);
            if (enterprisesUsersMapper.updateById(userEnterpriseRelation) <= 0) {
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.ENTERPRISE_SAVE_FAILED);
            }
        }
        //修改部门 新平台暂时不需要跟部门相关的功能
        /*List<Integer> add = new ArrayList<>();
        List<Integer> delete = new ArrayList<>();
        //获取该企业下的部门
        List<Departments> old_dep = departmentMapper.selectList(new EntityWrapper<Departments>().eq("org_id", org_id));
        if(CollectionUtils.isEmpty(old_dep)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        List<Integer> old_dep_id = new ArrayList<>();
        for (Departments department : old_dep) {
            UsersDepartments usersDepartments = usersDepartmentsService.selectOne(
                    new EntityWrapper<UsersDepartments>().eq("user_id", id).eq("department_id", department.getId()));
            if (!ObjectUtils.isEmpty(usersDepartments)) {
                old_dep_id.add(usersDepartments.getDepartmentId());
            }
        }
        //删除部门关系
        for (int i = 0; i < old_dep_id.size(); i++) {
            for (int j = 0; j < dep_ids.length; j++) {
                if (old_dep_id.get(i).equals(dep_ids[j])) {
                    break;
                }
            }
            delete.add(old_dep_id.get(i));
        }

        for (Integer dep_id : delete) {
            if(usersDepartmentsMapper.delete(
                    new EntityWrapper<UsersDepartments>().eq("department_id",dep_id).eq("user_id",id))<=0){
                throw new BusinessException(ResCodeEnum.SAVE_FAILED,ExceptionMessageEnum.ENTERPRISE_DELETE_USER_FAILED);
            }

        }

        //添加部门关系
        for (Integer dep_id : dep_ids) {
            if (ObjectUtils.isEmpty(departmentMapper.selectById(dep_id))) {
                return ResponseResult.buildResponseResult(ResCodeEnum.DEPARTMENT_NOT_EXIST);
            } else {
                UsersDepartments usersDepartment = usersDepartmentsService.selectOne(
                        new EntityWrapper<UsersDepartments>().eq("user_id", id).eq("department_id", dep_id));
                if (ObjectUtils.isEmpty(usersDepartment)) {
                    add.add(dep_id);
                }
            }
        }
        if (add.size() != 0) {
            for (Integer dep_id : add) {
                UsersDepartments departmentRelation = new UsersDepartments();
                departmentRelation.setUserId(id);
                departmentRelation.setDepartmentId(dep_id);
                if(usersDepartmentsMapper.insert(departmentRelation)<=0){
                    throw new BusinessException(ResCodeEnum.SAVE_FAILED,ExceptionMessageEnum.DEPARTMENT_SAVE_FAILED);
                }
            }
        }*/
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult ToDetermineWhetherTheSameBusiness(int[] ids) {
        if (ids.length == 0) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        } else if (ids.length < 2) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_MUST_GT_TWO);
        }
        //验证ids有效性
        for (int id : ids) {
            User user = usersMapper.selectById(id);
            if (ObjectUtils.isEmpty(user)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.GET_USER_FAILED);
            }
        }
        List<Integer> results = new ArrayList<>();
        boolean flag;
        for (int i = 0; i < ids.length - 1; i++) {
            //获取前一个用户的所有企业
            List<Integer> org_ids1 = new ArrayList<>();
            List<EnterprisesUsers> userEnterpriseRelations1 = enterprisesUsersMapper.selectList(
                    new EntityWrapper<EnterprisesUsers>().eq("user_id", ids[i]));
            if (CollectionUtils.isEmpty(userEnterpriseRelations1)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
            }
            for (EnterprisesUsers relation : userEnterpriseRelations1) {
                org_ids1.add(relation.getEnterpriseId());
                System.err.println("user id: " + ids[i] + " org id: " + relation.getEnterpriseId());
            }

            for (int j = i + 1; j < ids.length; j++) {
                //获取后一个用户的所有企业
                List<Integer> org_ids2 = new ArrayList<>();
                List<EnterprisesUsers> userEnterpriseRelations2 = enterprisesUsersMapper.selectList(
                        new EntityWrapper<EnterprisesUsers>().eq("user_id", ids[j]));
                for (EnterprisesUsers relation : userEnterpriseRelations2) {
                    org_ids2.add(relation.getEnterpriseId());
                    System.err.println("user id: " + ids[j] + " org id: " + relation.getEnterpriseId());
                }
                results.clear();
                flag = false;
                //比较两个用户是否有相同企业
                for (int k = 0; k < org_ids1.size(); k++) {
                    for (int g = 0; g < org_ids2.size(); g++) {
                        if (org_ids1.get(k).equals(org_ids2.get(g))) {
                            results.add(1);
                        } else {
                            results.add(0);
                        }
                    }
                    if (k != (org_ids1.size() - 1)) {
                        continue;
                    }
                    for (int result : results) {
                        if (result == 1) {
                            flag = true;
                            break;
                        }
                    }
                    if (flag) {
                        break;
                    }
                    //不在同一个企业
                    return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS).setObject(0);
                }
            }
        }
        //在同一个企业
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS).setObject(1);
    }

    @Override
    public ResponseResult<List<Departments>> getByEnterpriseId(Integer enterpriseId, AuthPlatformUserInfo userInfo) {
        if (ObjectUtils.isEmpty(enterpriseId)) {
            enterpriseId = userInfo.getOrgId();
        }
        List<Departments> departments = departmentsMapper.selectList(new EntityWrapper<>(new Departments().setOrgId(enterpriseId)));

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, departments);
    }

    /**
     * @param enterpriseId :企业id
     * @notes: 将部门遍历成一棵树
     * @Author: junyang.li
     * @Date: 16:14 2019/6/26
     * @return: java.util.List<com.bee.platform.user.dto.DepartmentTreeDTO>
     */
    @Override
    public List<DepartmentTreeDTO> departmentTree(Integer enterpriseId, AuthPlatformUserInfo userInfo) {
        List<Departments> departments = getByEnterpriseId(enterpriseId, userInfo).getObject();
        if (CollectionUtils.isEmpty(departments)) {
            return new ArrayList<>();
        }
        Map<Integer, DepartmentTreeDTO> map = new HashMap<>(16);
        List<DepartmentTreeDTO> list = new ArrayList<>();
        for (Departments item : departments) {
            DepartmentTreeDTO dto = BeanUtils.copyProperties(item, DepartmentTreeDTO.class);
            map.put(dto.getId(), dto);
            list.add(dto);
        }
        //遍历成树
        List<DepartmentTreeDTO> tree = new ArrayList<>();
        list.forEach(obj -> {
            if (obj.getId().equals(obj.getTreeId())) {
                tree.add(obj);
            } else {
                DepartmentTreeDTO dto = map.get(obj.getTreeId());
                if (null != dto) {
                    if (dto.getChildren() == null) {
                        dto.setChildren(new ArrayList<>());
                    }
                    dto.getChildren().add(obj);
                }
            }
        });
        return tree;
    }


    @Override
    public ResponseResult<List<DepartmentTreeDTO>> getEnterpriseDepartmentTree(Integer enterpriseId) {
        List<Departments> departments = departmentsMapper.selectList(new EntityWrapper<>(new Departments().setOrgId(enterpriseId)));
        List<DepartmentTreeDTO> treeDTOS = BeanUtils.assemble(DepartmentTreeDTO.class, departments);
        List<DepartmentTreeDTO> oneTrees = treeDTOS.stream().filter(a -> a.getLevel().equals(EnumEnterpriseLevel.levelNode.first_level.getKey())).collect(Collectors.toList());
        List<DepartmentTreeDTO> twoTrees = treeDTOS.stream().filter(a -> a.getLevel().equals(EnumEnterpriseLevel.levelNode.second_level.getKey())).collect(Collectors.toList());
        List<DepartmentTreeDTO> threeTrees = treeDTOS.stream().filter(a -> a.getLevel().equals(EnumEnterpriseLevel.levelNode.third_level.getKey())).collect(Collectors.toList());
        List<DepartmentTreeDTO> fourTrees = treeDTOS.stream().filter(a -> a.getLevel().equals(EnumEnterpriseLevel.levelNode.fourth_level.getKey())).collect(Collectors.toList());
        List<DepartmentTreeDTO> fiveTrees = treeDTOS.stream().filter(a -> a.getLevel().equals(EnumEnterpriseLevel.levelNode.fifth_level.getKey())).collect(Collectors.toList());
        for (DepartmentTreeDTO one : oneTrees) {
            buildDepartmentTree(one, treeDTOS);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, oneTrees);
    }

    /**
     * @param list :
     * @notes: 初始化数据时调整Departments表中的level字段
     * @Author: junyang.li
     * @Date: 15:07 2019/6/3
     * @return: void
     */
    @Override
    public void updateLevelByIds(List<Departments> list) {
        departmentsMapper.updateLevelByIds(list);
    }

    private DepartmentTreeDTO buildDepartmentTree(DepartmentTreeDTO one, List<DepartmentTreeDTO> treeDTOS) {
        one.setChildren(Lists.newArrayList());
        List<DepartmentTreeDTO> copy = BeanUtils.assemble(DepartmentTreeDTO.class, treeDTOS);
        Iterator<DepartmentTreeDTO> iterator = copy.iterator();
        while (iterator.hasNext()) {
            DepartmentTreeDTO next = iterator.next();
            if (one.getId().equals(next.getTreeId())) {
                iterator.remove();
                buildDepartmentTree(next, copy);
                one.getChildren().add(next);
            }
        }
        return one;
    }

    /**
     * 根据部门id查询 父级部门
     */
    @Override
    public List<Departments> getParentDaparment(Integer id) {
        Departments departments = this.selectOne(new EntityWrapper<>(new Departments().setId(id)));
        if (ObjectUtils.isEmpty(departments)) {
            return new ArrayList<>(0);
        }
        List<Departments> list = Lists.newArrayList();
        getParent(departments, list);
        if (!CollectionUtils.isEmpty(list)) {
            list.sort(Comparator.comparingInt(Departments::getLevel));
        }
        return list;
    }

    /**
     * 根据公司id和用户id查询下属用户id
     *
     * @param userId
     * @param orgId
     * @return
     */
    @Override
    public ResponseResult<Set<Integer>> getSubordinates(Integer userId, Integer orgId) {
        Set<Integer> subordinates = Sets.newHashSet();
        try {

            // 获取公司下所有部门
            List<Departments> allDepartments = departmentMapper.selectList(new EntityWrapper<Departments>().eq("org_id", orgId));
            List<AuthPlatformUserEnterprise> departments = authPlatformUserEnterpriseMapper.selectList(new EntityWrapper<AuthPlatformUserEnterprise>(new AuthPlatformUserEnterprise().setEnterpriseId(orgId).setUserId(userId).setDeleted(Status.FALSE.getKey())));
            ArrayList<Integer> subDepartments = Lists.newArrayList();
            if (!CollectionUtils.isEmpty(departments)) {
                Set<Integer> departIds = departments.stream().map(o -> o.getDepartmentsId()).collect(Collectors.toSet());
                List<Departments> childDepartments = departmentMapper.selectList(new EntityWrapper<Departments>(new Departments().setOrgId(orgId)).notIn("id", departIds).in("tree_id", departIds));
                childDepartments.stream()
                        .forEach(d -> {
                            subDepartments.add(d.getId());
                            iterDescendants(subDepartments, d.getId(), allDepartments);
                        });
            }
            if (!CollectionUtils.isEmpty(subDepartments)) {
                List<AuthPlatformUserEnterprise> list = authPlatformUserEnterpriseMapper
                        .selectList(new EntityWrapper<AuthPlatformUserEnterprise>().in("departments_id", subDepartments)
                                .eq("enterprise_id", orgId).eq("deleted", Status.FALSE.getKey()));
                if (!org.springframework.util.CollectionUtils.isEmpty(list)) {
                    subordinates = list.stream().map(ue -> ue.getUserId()).collect(Collectors.toSet());
                }
            }

        } catch (Exception e) {
            log.error("query occur error", e);
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED, null);
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, subordinates);
    }

    private List<Departments> getParent(Departments departments, List<Departments> list) {
        Integer id = departments.getId();
        Integer treeId = departments.getTreeId();
        if (!id.equals(treeId)) {
            Departments parent = this.selectOne(new EntityWrapper<>(new Departments().setId(treeId)));
            if (!ObjectUtils.isEmpty(parent)) {
                list.add(parent);
                getParent(parent, list);
            }
        }
        return list;
    }

    /**
     * 校验是否存在同名的同级部门
     *
     * @param orgId
     * @param pid
     * @param id
     * @param name
     * @return
     */
    private boolean checkDuplicateDepartmentName(Integer orgId, Integer pid, Integer id, String name) {
        Wrapper<Departments> wrapper =
                new EntityWrapper<Departments>().eq("name", name);

        // 编辑用户时
        if (!ObjectUtils.isEmpty(id)) {
            wrapper.ne("id", id);
        }

        // 添加1级部门校验条件
        if (ObjectUtils.isEmpty(pid)) {
            wrapper.eq("level", 1).eq("org_id", orgId);
        }
        // 添加2级部门校验条件
        else {
            wrapper.eq("tree_id", pid).ne("id", pid);
        }
        Integer count = departmentMapper.selectCount(wrapper);
        return count == 0;
    }

    /**
     * 迭代获取自己和所有后代部门id
     */
    private void iterDescendants(List<Integer> descenIds, Integer id, List<Departments> allDepartments) {
        // 查询下级部门
        List<Departments> departments = allDepartments.stream().filter(d -> id.equals(d.getTreeId()) && !id.equals(d.getId())).collect(Collectors.toList());
        allDepartments.removeAll(departments);
        if (!CollectionUtils.isEmpty(departments)) {
            departments.forEach(d -> {
                descenIds.add(d.getId());
                iterDescendants(descenIds, d.getId(), allDepartments);
            });
        }
    }

    @Override
    public ResponseResult<AuthUsergroupDeparmentTreeDTO> getUsergroupDeparmentTree(int orgId) {
        AuthEnterprise enterprise = enterprisesMapper.selectOne(new AuthEnterprise().setId(orgId).setDeleted(Status.FALSE.getKey()));
        if (ObjectUtils.isEmpty(enterprise)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        AuthUsergroupDeparmentTreeDTO dto = new AuthUsergroupDeparmentTreeDTO();
        dto.setDepartmentId(enterprise.getId())
                .setTreeId(enterprise.getId())
                .setDepartment(enterprise.getName())
                .setChildren(Lists.newArrayList());
        // 查询企业的所有部门
        List<Departments> departmentList = this.selectList(new EntityWrapper<>(new Departments().setOrgId(orgId)));
        // 企业的所有一级部门
        List<Departments> topDepartmentList = Lists.newArrayList();
        if (!CollectionUtils.isEmpty(departmentList)) {
            topDepartmentList = departmentList.stream().filter(a -> Objects.equals(Status.TRUE.getKey(), a.getLevel())).collect(Collectors.toList());
        }
        // 查询一级部门下所有部门
        for (Departments top : topDepartmentList) {
            AuthUsergroupDeparmentTreeDTO treeDTO = new AuthUsergroupDeparmentTreeDTO()
                    .setDepartmentId(top.getId())
                    .setTreeId(top.getTreeId())
                    .setDepartment(top.getName());
            AuthUsergroupDeparmentTreeDTO child = buildUsergroupDeparmentTree(treeDTO, departmentList);
            dto.getChildren().add(child);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    /**
     * 建用户组--部门树
     *
     * @param dto
     * @param departmentsList
     * @return
     */
    private AuthUsergroupDeparmentTreeDTO buildUsergroupDeparmentTree(AuthUsergroupDeparmentTreeDTO dto, List<Departments> departmentsList) {
        for (Departments d : departmentsList) {
            // id等于儿子treeId 而且id和treeId不等
            if (d.getTreeId().equals(dto.getDepartmentId()) && !d.getId().equals(d.getTreeId())) {
                if (CollectionUtils.isEmpty(dto.getChildren())) {
                    dto.setChildren(Lists.newArrayList());
                }
                AuthUsergroupDeparmentTreeDTO treeDTO = new AuthUsergroupDeparmentTreeDTO()
                        .setDepartmentId(d.getId())
                        .setTreeId(d.getTreeId())
                        .setDepartment(d.getName());
                buildUsergroupDeparmentTree(treeDTO, departmentsList);
                dto.getChildren().add(treeDTO);
            }
        }
        return dto;
    }

    @Override
    public ResponseResult<List<AuthUsergroupUserListDTO>> getUsergroupDeparmentUsers(int orgId, int departmentId) {
        List<AuthPlatformUserEnterprise> userEnterpriseList = authPlatformUserEnterpriseService.selectList(new EntityWrapper<>(new AuthPlatformUserEnterprise()
                .setDepartmentsId(departmentId)
                .setEnterpriseId(orgId)
                .setDeleted(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey())));
        if (CollectionUtils.isEmpty(userEnterpriseList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        List<Integer> userIds = userEnterpriseList.stream().map(a -> a.getUserId()).collect(Collectors.toList());
        List<AuthPlatformUser> userList = userService.selectList(new EntityWrapper<>(new AuthPlatformUser()
                .setStatus(Status.TRUE.getKey())
                .setDeleted(Status.FALSE.getKey())).in("id", userIds));
        List<AuthUsergroupUserListDTO> resultList = Lists.newArrayList();
        for (AuthPlatformUser user : userList) {
            resultList.add(new AuthUsergroupUserListDTO()
                    .setUserId(user.getId())
                    .setName(user.getName()));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resultList);
    }

    @Override
    public List<Departments> getParentAndSelf(Integer departmentId, Integer orgId) {
        Departments department = this.selectOne(new EntityWrapper<>(new Departments().setId(departmentId)));
        // 如果不存在直接返回空
        if (ObjectUtils.isEmpty(department)) {
            return new ArrayList<>(0);
        }
        // 如果没有父级  则返回自身
        if (Objects.equals(department.getId(), department.getTreeId())) {
            List<Departments> list = new ArrayList<>(2);
            list.add(department);
            return list;
        }
        // 如果有父级则返回父级和自身
        List<Departments> departmentsList = this.selectList(new EntityWrapper<>(new Departments().setOrgId(orgId)));
        List<Departments> resultList = Lists.newArrayList();
        getParent(department, departmentsList, resultList);
        return resultList;
    }

    private void getParent(Departments department, List<Departments> departmentList, List<Departments> resultList) {
        resultList.add(department);
        if (Objects.equals(department.getId(), department.getTreeId())) {
            return;
        }
        for (Departments d : departmentList) {
            if (Objects.equals(d.getId(), department.getTreeId())) {
                getParent(d, departmentList, resultList);
            }
        }
        resultList.sort(Comparator.comparingInt(Departments::getLevel));
    }
}

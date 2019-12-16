package com.bee.platform.user.service.impl;

import com.alibaba.fastjson.JSONObject;
import com.aliyuncs.exceptions.ClientException;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.bcrypt.BCryptPassword;
import com.bee.platform.common.constants.enums.EnumMiddleNoticeTitle;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.DepartmentInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.MiddleNoticeContentTemplate;
import com.bee.platform.common.enums.NoticeTemplateType;
import com.bee.platform.common.enums.RoleType;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.Validator;
import com.bee.platform.user.constants.enums.EnumEnterpriseUser;
import com.bee.platform.user.dao.mapper.*;
import com.bee.platform.user.dto.EnterprisesUsersDTO;
import com.bee.platform.user.dto.EnterprisesUsersInfoDTO;
import com.bee.platform.user.dto.EnterprisesUsersListDTO;
import com.bee.platform.user.dto.EnterprisesUsersRQ;
import com.bee.platform.user.entity.*;
import com.bee.platform.user.service.*;
import com.bee.platform.user.sms.SmsService;
import com.bee.platform.user.utils.ValidateUtils;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

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
public class EnterprisesUsersServiceImpl extends ServiceImpl<EnterprisesUsersMapper, EnterprisesUsers> implements EnterprisesUsersService {

    @Autowired
    private EnterprisesUsersService enterprisesUsersService;

    @Autowired
    private EnterprisesUsersMapper enterprisesUsersMapper;

    @Autowired
    private UsersService usersService;

    @Autowired
    private UserMapper userMapper;

    @Autowired
    private DepartmentsServiceImpl departmentsService;

    @Autowired
    private UsersDepartmentsService usersDepartmentsService;

    @Autowired
    private UsersRolesService usersRolesService;

    @Autowired
    private ZPostMapper zPostMapper;

    @Autowired
    private SmsService smsService;

    @Autowired
    private SystemNoticeMapper systemNoticeMapper;

    @Autowired
    private UsersDepartmentsMapper usersDepartmentsMapper;


    @Autowired
    private MiddleSystemNoticeService middleSystemNoticeService;

    @Autowired
    private ConfigService configService;

    @Autowired
    private DepartmentsMapper departmentsMapper;

    @Autowired
    private  EnterprisesMapper enterprisesMapper;

    @Autowired
    private UsersRolesMapper usersRolesMapper;

    @Override
    public EnterprisesUsers findByUserIdAndEnterpriseId(Integer userId, Integer enterpriseId) {

        Wrapper<EnterprisesUsers> wrapper = new EntityWrapper<EnterprisesUsers>();
        wrapper.eq("user_id", userId).eq("enterprise_id", enterpriseId);
        return selectOne(wrapper);
    }

    @Override
    public ResponseResult<EnterprisesUsersDTO> getEnterpriseUserInfoById(Integer userId, Integer enterpriseId) {
        User user = usersService.selectById(userId);
        if (user == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.USER_NOT_EXIST);
        }
        //查询用户在该企业下的相关信息
        EnterprisesUsers enterprisesUsers = enterprisesUsersService.selectOne(
                new EntityWrapper<EnterprisesUsers>().eq("user_id", userId).eq("enterprise_id", enterpriseId));
        if (enterprisesUsers == null) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_USER_NOT_FOUND);
        }
        EnterprisesUsersDTO dto = BeanUtils.copyProperties(enterprisesUsers, EnterprisesUsersDTO.class).setUsername(user.getUsername());
        //查询所在部门
        DepartmentInfo department = departmentsService.selectByUserIdAndOrgId(userId, enterpriseId);
        if (department != null) {
            dto.setDepartment(department);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    /**
     * @param ids EnterprisesUsers表主键
     * @notes 每天凌晨重置用户被邀请的状态
     * @Author junyang.li
     * @Date 16:30 2019/3/20
     **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateInvite(List<Integer> ids) {
        enterprisesUsersMapper.updateInvite(ids);
    }

    /**
     * @notes 查询该用户的企业id
     * @Author junyang.li
     * @Date 17:07 2019/1/18
     **/
    @Override
    public List<Integer> userInEnterprises(Integer userId, List<Integer> orgIds) {
        List<EnterprisesUsers> enterprisesUsers = baseMapper.selectList(new EntityWrapper<EnterprisesUsers>()
                .where("user_id={0}", userId).and().in("enterprise_id", orgIds));
        if (CollectionUtils.isEmpty(enterprisesUsers)) {
            new ArrayList<>();
        }
        return enterprisesUsers.stream().map(EnterprisesUsers::getEnterpriseId).collect(Collectors.toList());
    }

   /* @Override
    public List<Enterprises> listByUserId(Integer userId) {
        List<Enterprises> enterprises = baseMapper.listByUserId(userId);
        return null;
    }*/

    /**
     * 把用户添加到企业用户列表
     */
    @Override
    public ResponseResult<Object> add(EnterprisesUsersInfoDTO enterprisesUsersInfoDTO, AuthPlatformUserInfo userInfo) {
        EnterprisesUsers enterprisesUsers = new EnterprisesUsers();
        if (!StringUtils.isEmpty(enterprisesUsersInfoDTO)) {
            String phone = enterprisesUsersInfoDTO.getPhone();
            if (null == phone) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
            }
            if (!Validator.isMobile(phone)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
            }
            String enterprisesUsersPhone = enterprisesUsersMapper.selectEnterprisesUsersPhone(phone);
            if (enterprisesUsersPhone != null) {
                return ResponseResult.buildResponseResult(ResCodeEnum.USER_AT_ALREADY);
            }
            ZPost zPost = zPostMapper.selectOne(new ZPost().setId(enterprisesUsersInfoDTO.getZpostid()));
            if (null == zPost) {
            	return ResponseResult.buildResponseResult(ResCodeEnum.POST_NOT_EXIST);
			}
            String name = zPost.getName();
            User platformUser = null;
            Integer userId = 0;
            BeanUtils.copyProperties(enterprisesUsersInfoDTO, enterprisesUsers);
            // User user = usersService.selectOne(new User().setUsername(phone));
            // 获取当前操作人的企业ID
            Integer orgId = userInfo.getOrgId();
            // 根据电话号码查询用户列表的用户手机号码
            String userByPhone = userMapper.selectUserByPhone(phone);
            // 是平台用户 如果查询出来的电话为空就要在平台用户列表插入一条数据
            if (userByPhone != null) {
                platformUser = usersService.selectOne(new User().setUsername(phone));
                userId = platformUser.getId();
                //把平台用户ID和企业用户ID关联
                enterprisesUsers.setUserId(userId)
                        .setEnterpriseId(orgId);
                enterprisesUsersService.insert(enterprisesUsers);
                //发被操作人
                Enterprises enterprises = enterprisesMapper.selectOne(new Enterprises().setId(orgId));
                middleSystemNoticeService.createNotice(userId,EnumMiddleNoticeTitle.title.ADD_USER_SUCCESS.getValue(),
                        MiddleNoticeContentTemplate.ENTERPRISES_ADD_USER.getKey(),userInfo.getName(),enterprises.getName());
                //发给管理员
                middleSystemNoticeService.createNotice(userInfo.getId(),EnumMiddleNoticeTitle.title.ADD_USER_SUCCESS.getValue(),
                        MiddleNoticeContentTemplate.ENTERPRISES_ADD_USER_MANAGER.getKey(),enterprisesUsersInfoDTO.getNickname(),enterprises.getName());
            }
            // 添加用户不是平台用户
            else {
                String defaultPassword = configService.getConfValue(ConstantsUtil.DEFAULT_PASSWORD, "123456", "系统配置的默认密码");
                User user = new User()
                        .setUuid(UUID.randomUUID().toString().replaceAll("-", ""))
                        .setUsername(phone)
                        .setPassword(BCryptPassword.encode(defaultPassword))
                        .setEmail(enterprisesUsersInfoDTO.getEmail())
                        .setPhone(phone).setNickname(enterprisesUsersInfoDTO.getNickname())
                        .setCreateAt(new Date())
                        .setUpdateAt(new Date());
                usersService.insert(user);
                platformUser= userMapper.selectOne(new User().setPhone(phone));
                 //获取用户ID
                if (!(null == platformUser)) {
                	userId = platformUser.getId();
				}

                // 在企业用户表中关联平台用户ID
                enterprisesUsers.setUserId(userId)
                        .setIsActive(1)
                        .setIsInvite(1)
                        .setPost(name)
                        .setZpostid(enterprisesUsersInfoDTO.getZpostid())
                        .setEnterpriseId(orgId);
                enterprisesUsersService.insert(enterprisesUsers);
                UsersDepartments usersDepartments = new UsersDepartments()
                        .setDepartmentId(enterprisesUsersInfoDTO.getDepartmentsid())
                        .setUserId(userId)
                        .setPost(name)
                        .setPostId(enterprisesUsersInfoDTO.getZpostid());
                usersDepartmentsService.insert(usersDepartments);
                UserRole userRole = new UserRole()
                        .setUserId(userId)
                        .setOrgId(orgId)
                        //默认是普通用户角色
                        .setRoleId(RoleType.USER.getCode());
                usersRolesService.insert(userRole);
                 //保存消息
                Enterprises enterprises = enterprisesMapper.selectOne(new Enterprises().setId(orgId));
                middleSystemNoticeService.createNotice(userInfo.getId(),EnumMiddleNoticeTitle.title.ENTERPRISE_RELATION_APPLY.getValue(),
                        MiddleNoticeContentTemplate.ENTERPRISES_ADD_USER.getKey(),enterprisesUsersInfoDTO.getNickname(),enterprises.getName());

            }
            // 成功添加新用户向被操作人发送短信
            this.sendSmsAfterAddUser(phone, userInfo.getName(), userInfo.getOrg_name(),
                    NoticeTemplateType.ENTERPRISE_ADD_USER.getKey());

            //保存企业完成添加用户消息
            middleSystemNoticeService.createNotice(userId,
                    EnumMiddleNoticeTitle.title.ADD_USER_SUCCESS.getValue(),
                    MiddleNoticeContentTemplate.ENTERPRISES_ADD_USER.getKey(),
                    userInfo.getName(), userInfo.getOrg_name());
            String nickName = enterprisesUsersInfoDTO.getPhone();
            if (Objects.nonNull(platformUser) && Objects.nonNull(platformUser.getNickname())) {
                nickName = platformUser.getNickname();
            }
            middleSystemNoticeService.createNotice(userInfo.getId(),
                    EnumMiddleNoticeTitle.title.ADD_USER_SUCCESS.getValue(),
                    MiddleNoticeContentTemplate.ENTERPRISES_ADD_USER_MANAGER.getKey(),
                    nickName, userInfo.getOrg_name());

            //企业管理员完成用户添加后，即生成企业关联申请，此时保存企业关联申请消息
            middleSystemNoticeService.createNotice(userId,
                    EnumMiddleNoticeTitle.title.ENTERPRISE_RELATION_APPLY.getValue(),
                    MiddleNoticeContentTemplate.ENTERPRISE_RELATE_APPLY.getKey(),
                    userInfo.getOrg_name());

        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 根据电话或姓名职务部门查询企业用户列表
     */
    @Override
    public ResponseResult<ArrayList<Object>> query(AuthPlatformUserInfo userInfo, EnterprisesUsersRQ rq, Pagination pagination) {
        //ZPost zPost = zPostMapper.selectById(rq.getZpostid());
        ArrayList<Object> list = new ArrayList<>();
        Map<String, Object> paramMap = new HashMap<>(16);
        List<EnterprisesUsers> enterprisesUsersList = null;
        usersRolesMapper.selectOne(new UserRole().setUserId(userInfo.getId()));
        Integer orgId = userInfo.getOrgId();
        if (null == rq) {
            // 获取当前操作人的企业ID
        	enterprisesUsersList = enterprisesUsersMapper.selectByEnterpriseId(orgId, pagination);
        	if (CollectionUtils.isEmpty(enterprisesUsersList)) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
	        } else {
//	        	List<EnterprisesUsersListDTO> list =  BeanUtils.assemble(EnterprisesUsersListDTO.class, enterprisesUsersList);
	            transformationDTO(list, enterprisesUsersList);
	        }
		} else {
			if (!StringUtils.isEmpty(rq.getNameOrPhone())) {
	        	paramMap.put("nameOrPhone", rq.getNameOrPhone());
			}
	        if (!StringUtils.isEmpty(rq.getZpostid())) {
	        	paramMap.put("zpostid", rq.getZpostid());
			}
	        if (!StringUtils.isEmpty(rq.getDepartmentsid())) {
	        	paramMap.put("departmentsid", rq.getDepartmentsid());
			}
            paramMap.put("enterpriseId", orgId);
	        enterprisesUsersList = enterprisesUsersMapper.selectEnterprisesUsers(paramMap, pagination);

	        if (CollectionUtils.isEmpty(enterprisesUsersList)) {
	            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND, Lists.newArrayList(), PageUtils.transToPage(pagination));
	        } else {
	        	transformationDTO(list, enterprisesUsersList);
	        }
		}

      //获取部门树的字符串
       // String departmentLevelWithSelf = departmentsService.getDepartmentLevelWithSelf(rq.getDepartmentsid());

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,PageUtils.transToPage(pagination));
    }
    /**
     *
     * @param list
     * @param enterprisesUsersList
     */
	private void transformationDTO(ArrayList<Object> list, List<EnterprisesUsers> enterprisesUsersList) {
        List<Integer> departmentIds=new ArrayList<>();
		for (EnterprisesUsers enterprisesUsers : enterprisesUsersList) {
		    EnterprisesUsersListDTO enterprisesUsersListDTO = new EnterprisesUsersListDTO();
		    BeanUtils.copyProperties(enterprisesUsers, enterprisesUsersListDTO)
                            .setPost(enterprisesUsers.getPost()).setDepartmentId(enterprisesUsers.getDepartmentsid());
            departmentIds.add(enterprisesUsers.getDepartmentsid());
            if (null==(enterprisesUsers.getDepartmentsid())){
                enterprisesUsersListDTO.setDepartmentName(null)
                        .setPost(enterprisesUsers.getPost());
                list.add(enterprisesUsersListDTO);
            }else {
                Departments departments = departmentsMapper.selectOne(new Departments().setId(enterprisesUsers.getDepartmentsid()));
                enterprisesUsersListDTO.setDepartmentName(departments.getName())
                        .setPost(enterprisesUsers.getPost());
                list.add(enterprisesUsersListDTO);
            }

		}
	}

    /**
     * 模糊查询企业用户列表
     */
    @Override
    public List<EnterprisesUsersListDTO> queryAll(AuthPlatformUserInfo userInfo, Pagination pagination) {
        // 获取当前操作人的企业ID
        Integer orgId = userInfo.getOrgId();
        List<EnterprisesUsersListDTO> list = new ArrayList<>();
        EnterprisesUsersListDTO enterprisesUsersListDTO = new EnterprisesUsersListDTO();
        List<EnterprisesUsers> enterprisesUsersList = enterprisesUsersMapper.selectByEnterpriseId(orgId, pagination);
        List<Integer> departmentIds=new ArrayList<>();
        for (EnterprisesUsers enterprisesUsers : enterprisesUsersList) {
            BeanUtils.copyProperties(enterprisesUsers, enterprisesUsersListDTO)
            .setPost(enterprisesUsers.getPost()).setDepartmentId(enterprisesUsers.getDepartmentsid());
            departmentIds.add(enterprisesUsers.getDepartmentsid());
            list.add(enterprisesUsersListDTO);
        }
        List<Departments> departments= departmentsService.selectList(new EntityWrapper<Departments>().in("id",departmentIds));
        if(CollectionUtils.isEmpty(departments)){
            return list;
        }
        Map<Integer,String> map=new HashMap<>(16);
        departments.forEach(obj->map.put(obj.getId(),obj.getName()));
        list.forEach(obj->obj.setDepartmentName(map.get(obj.getDepartmentId())));
        return list;

    }

    /**
     * 企业用户是否被禁用
     * id 是企业用户ID
     */
    @Override
    public ResponseResult<Object> upDateById(int id, Integer isActive, AuthPlatformUserInfo userInfo) {
        EnterprisesUsers enterprisesUsers = enterprisesUsersMapper.selectById(id);
        boolean updateFlag = false;
        if (StringUtils.isEmpty(enterprisesUsers)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        } else {
            User user = userMapper.selectById(enterprisesUsers.getUserId());
            user.setIsActive(isActive);
            usersService.updateById(user);
            enterprisesUsers.setIsActive(isActive);

            updateFlag = enterprisesUsersService.updateById(enterprisesUsers);
            if (updateFlag) {
                //企业管理员禁用和启用用户账号后，向该用户发送短信
                // 企业管理员启用
                if (EnumEnterpriseUser.ActiveType.is_active.getKey().equals(isActive)) {
                    this.sendSmsAfterIsActiveUser(user.getPhone(), userInfo.getName(),
                            NoticeTemplateType.ENTERPRISE_ADMIN_ENABLE_ACCOUNT.getKey());
                    //保存提示信息
                    middleSystemNoticeService.createNotice(enterprisesUsers.getUserId(),
                            EnumMiddleNoticeTitle.title.USER_STATUS_CHANGE.getValue(),
                            MiddleNoticeContentTemplate.ENTERPRISE_ADMINISTRATORS_ENABLE_ACCOUNT.getKey(),
                            new Object[] {userInfo.getName()});
                    middleSystemNoticeService.createNotice(userInfo.getId(),
                            EnumMiddleNoticeTitle.title.USER_STATUS_CHANGE.getValue(),
                            MiddleNoticeContentTemplate.ENTERPRISE_ADMINISTRATORS_ENABLE_ACCOUNT.getKey(),
                            new Object[] {user.getNickname()});
                }
                // 企业管理员禁用
                if (EnumEnterpriseUser.ActiveType.not_active.getKey().equals(isActive)) {
                    this.sendSmsAfterIsActiveUser(user.getPhone(), userInfo.getName(),
                            NoticeTemplateType.ENTERPRISE_ADMIN_DISABLE_ACCOUNT.getKey());
                    //保存提示信息
                    middleSystemNoticeService.createNotice(enterprisesUsers.getUserId(),
                            EnumMiddleNoticeTitle.title.USER_STATUS_CHANGE.getValue(),
                            MiddleNoticeContentTemplate.ENTERPRISE_ADMINISTRATORS_DISABLE_ACCOUNT.getKey(),
                            new Object[] {userInfo.getName()});
                    middleSystemNoticeService.createNotice(userInfo.getId(),
                            EnumMiddleNoticeTitle.title.USER_STATUS_CHANGE.getValue(),
                            MiddleNoticeContentTemplate.ENTERPRISE_ADMINISTRATORS_DISABLE_ACCOUNT.getKey(),
                            new Object[] {user.getNickname()});
                }
            }

        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, updateFlag);

    }

    /**
     * 编辑企业用户
     * id 是企业用户ID
     */
    @Override
    public ResponseResult<Object> upDateByIdEnterprisesUsers(AuthPlatformUserInfo userInfo, EnterprisesUsersInfoDTO enterprisesUsersInfoDTO) {
        EnterprisesUsers enterprisesUsers = enterprisesUsersMapper.selectOne(new EnterprisesUsers().setId(enterprisesUsersInfoDTO.getId()));
        if (StringUtils.isEmpty(enterprisesUsers)) {
        	return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
		}
        if (StringUtils.isEmpty(enterprisesUsers)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        } else {
            if (!StringUtils.isEmpty(enterprisesUsersInfoDTO)) {
                BeanUtils.copyProperties(enterprisesUsersInfoDTO, enterprisesUsers);
                UsersDepartments usersDepartments = usersDepartmentsMapper.selectOne(new UsersDepartments()
                        .setDepartmentId(enterprisesUsers.getDepartmentsid())
                        .setUserId(enterprisesUsers.getUserId()));
                if (!(null == enterprisesUsersInfoDTO.getZpostid())) {
                    ZPost zPost = zPostMapper.selectOne(new ZPost()
                            .setId(enterprisesUsersInfoDTO.getZpostid()));
                    usersDepartments.setPostId(enterprisesUsersInfoDTO.getZpostid())
                            .setPost(zPost.getName());
                    usersDepartmentsService.updateById(usersDepartments);
                }
            }
        }
        boolean updateFlag = enterprisesUsersService.updateById(enterprisesUsers);
        if (updateFlag) {
            try {
                //短信通知企业用户
                JSONObject jsonObject = new JSONObject();
                jsonObject.put("enterprise_admin_username", userInfo.getNickname());
                smsService.sendMessageForPrompt(enterprisesUsers.getPhone(),
                        NoticeTemplateType.ENTERPRISE_ADMIN_UPDATE_USER_INFO.getKey(), jsonObject.toJSONString());
                //保存提示信息
                middleSystemNoticeService.createNotice(enterprisesUsers.getUserId(),
                        EnumMiddleNoticeTitle.title.USER_CHANGE_DATA_NOTICE.getValue(),
                        MiddleNoticeContentTemplate.ENTERPRISE_ADMIN_UPDATE_USER_INFO.getKey(),
                        new Object[] {userInfo.getName()});

                //短信通知企业管理员
                jsonObject = new JSONObject();
                jsonObject.put("username", enterprisesUsersInfoDTO.getNickname());
                smsService.sendMessageForPrompt(userInfo.getName(),
                        NoticeTemplateType.UPDATE_USER_INFO.getKey(), jsonObject.toJSONString());
                //保存提示信息
                middleSystemNoticeService.createNotice(enterprisesUsers.getUserId(),
                        EnumMiddleNoticeTitle.title.USER_CHANGE_DATA_NOTICE.getValue(),
                        MiddleNoticeContentTemplate.UPDATE_USER_INFO.getKey(),
                        new Object[] {enterprisesUsersInfoDTO.getNickname()});

            } catch (ClientException e) {
                log.error("手机提示企业管理员变更用户资料通知消息发送失败，手机号是:{}。错误信息是:{}", enterprisesUsers.getPhone(), e);
            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 判断手机号码是否被企业用户表注册
     */
    @Override
    public ResponseResult<Object> getEnterprisesUsers(String phone) {
        String enterprisesUsersPhone = enterprisesUsersMapper.selectEnterprisesUsersPhone(phone);
        if (!StringUtils.isEmpty(enterprisesUsersPhone)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PHONE_NUMBER_EXIST);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.PHONE_NO_NUMBER_EXIST);
    }

    /**
     * @Description 企业完成添加用户后发送短信
     * @author chenxm66777123
     * @Date 2019年5月9日
     * @version 1.0.0
     */
    private void sendSmsAfterAddUser(String phone, String enterpriseAdminUsername, String enterpriseName, Integer key) {
        try {
            if (!ValidateUtils.isPhone(phone)) {
                log.info(ResCodeEnum.NOT_PHONE_NUMBER.msg);
            }
            //组装参数
            JSONObject sendJson = new JSONObject();
            sendJson.put("enterprise_name", enterpriseName);
            sendJson.put("enterprise_admin_username", enterpriseAdminUsername);
            smsService.sendMessageForPrompt(phone, key, sendJson.toJSONString());
        } catch (Exception e) {
            log.info(e.getMessage());
        }
    }

    /**
     * @Description 企业管理员启用和禁用用户账号后向用户发送短信
     * @author chenxm66777123
     * @Date 2019年5月9日
     * @version 1.0.0
     */
    private void sendSmsAfterIsActiveUser(String phone, String enterpriseAdminUsername, Integer key) {
        try {
            if (!ValidateUtils.isPhone(phone)) {
                log.info(ResCodeEnum.NOT_PHONE_NUMBER.msg);
            }
            //组装参数
            JSONObject sendJson = new JSONObject();
            sendJson.put("enterprise_admin_username", enterpriseAdminUsername);
            smsService.sendMessageForPrompt(phone, key, sendJson.toJSONString());
        } catch (Exception e) {
            log.info(e.getMessage());
        }
    }

    /**
     * 编辑用户的时候要返回数据
     */
    @Override
    public ResponseResult<EnterprisesUsersInfoDTO> getEnterprisesUsersById(Integer id) {
        EnterprisesUsers enterprisesUsers = enterprisesUsersMapper.selectById(id);
        if(enterprisesUsers==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_USER_NOT_FOUND);
        }
        EnterprisesUsersInfoDTO enterprisesUsersInfoDTO = new EnterprisesUsersInfoDTO();
        User user = userMapper.selectOne(new User().setId(enterprisesUsers.getUserId()));
        enterprisesUsersInfoDTO.setHead(user.getHead());
        BeanUtils.copyProperties(enterprisesUsers, enterprisesUsersInfoDTO);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,enterprisesUsersInfoDTO);
	}

}

package com.bee.platform.user.authority.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.bcrypt.BCryptPassword;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.dto.IndustryDTO;
import com.bee.platform.common.entity.*;
import com.bee.platform.common.enums.EnumRoleType;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.enums.UserType;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.service.RegionService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.common.utils.Validator;
import com.bee.platform.user.authority.dao.mapper.AuthEnterpriseMapper;
import com.bee.platform.user.authority.dao.mapper.AuthPlatformUserEnterpriseMapper;
import com.bee.platform.user.authority.dao.mapper.AuthPlatformUserMapper;
import com.bee.platform.user.authority.dao.mapper.AuthUserRoleMapper;
import com.bee.platform.user.authority.dto.*;
import com.bee.platform.user.authority.dto.DepartmentDTO;
import com.bee.platform.user.authority.entity.*;
import com.bee.platform.user.authority.enums.EnumFileType;
import com.bee.platform.user.authority.enums.EnumNewCommon;
import com.bee.platform.user.authority.rq.*;
import com.bee.platform.user.authority.service.*;
import com.bee.platform.user.constants.enums.EnumEnterpriseCheck;
import com.bee.platform.user.constants.enums.EnumEnterpriseUser;
import com.bee.platform.user.dao.mapper.DepartmentsMapper;
import com.bee.platform.user.dto.*;
import com.bee.platform.user.entity.*;
import com.bee.platform.user.service.*;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import javax.annotation.Resource;
import java.lang.ref.SoftReference;
import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 企业表 服务实现类
 * </p>
 *
 * @author LILIANG
 * @since 2019-05-20
 */
@Slf4j
@Service
public class AuthEnterpriseServiceImpl extends ServiceImpl<AuthEnterpriseMapper, AuthEnterprise> implements AuthEnterpriseService {

    @Autowired
    private AuthEnterpriseMapper enterpriseMapper;
    @Autowired
    private JedisService jedisService;
    @Resource
    private RedisTemplate<String, Integer> redisTemplate;
    @Autowired
    private AuthUserRoleMapper userRoleMapper;
    @Autowired
    private AuthPlatformUserEnterpriseService userEnterpriseService;
    @Autowired
    private AuthPlatformUserEnterpriseMapper userEnterpriseMapper;
    @Autowired
    private AuthEnterpriseRoleService enterpriseRoleService;
    @Autowired
    private AuthCommonFileService commonFileService;
    @Autowired
    private AuthPlatformUserService userService;
    @Autowired
    private CommonRegionService commonRegionService;
    @Autowired
    private ConfigService configService;
    @Autowired
    private DepartmentsService departmentsService;
    @Autowired
    private ZPostService postService;
    @Autowired
    private IndustryService industryService;
    @Autowired
    private EnterprisesCheckService enterprisesCheckService;
    @Autowired
    private DepartmentsMapper departmentsMapper;
    @Autowired
    private AuthRoleService roleService;
    @Autowired
    private AuthPlatformUserMapper authPlatformUserMapper;
    @Autowired
    private RegionService regionService;


    /**
     * 企业编号生成规则：前四位redis自增序号，后四位：母公司为0000，子公司为母公司前四位
     * 每个企业最多只能添加5个子公司（删除的也算）
     * 充钱才能添加更多子公司（不充钱怎么变强？！）
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> addEnterprise(AuthEnterpriseAddRQ rq, AuthPlatformUserInfo userInfo) {
        // 判断企业名是否重复
        List<AuthEnterprise> enterprises = enterpriseMapper.selectList(new EntityWrapper<>(new AuthEnterprise()
                .setName(rq.getName())
                .setDeleted(EnumNewCommon.IsDeleted.not_Delete.getKey())));
        if (!CollectionUtils.isEmpty(enterprises)) {
            log.info("企业已注册，类是：{}，方法是：{}", "AuthEnterpriseServiceImpl", "add");
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_ALREADY_REGISTER);
        }
        // 校验手机号
        if (!Validator.isMobile(rq.getLinkman())) {
            log.error("企业管理员设置失败，联系人手机号不正确 类是：{}，方法是：{}", "AuthEnterpriseServiceImpl", "add");
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_PHONE_NUMBER);
        }
        // 判断子公司数目是否达到上限
        AuthEnterprise pEterprise = new AuthEnterprise();
        if (!ObjectUtils.isEmpty(rq.getId()) && rq.getId() != 0) {
            pEterprise = enterpriseMapper.selectOne(new AuthEnterprise().setId(rq.getId()));
            if (ObjectUtils.isEmpty(pEterprise)) {
                return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
            }
            Integer count = enterpriseMapper.selectCount(new EntityWrapper<>(new AuthEnterprise().setPid(rq.getId())));
            if (pEterprise.getChildNum().compareTo(count) < 0) {
                log.info("可添加子公司已达上限，类是：{}，方法是：{}", "AuthEnterpriseServiceImpl", "add");
                return ResponseResult.buildResponseResult(ResCodeEnum.CHILD_REACH_LIMIT);
            }
        }
        // 从rq中复制信息
        AuthEnterprise enterprise = BeanUtils.copyProperties(rq, AuthEnterprise.class);
        // 查询配置表中可以添加 子公司数目 添加到企业信息
        Config config = configService.getConfigByconfigKey(ConstantsUtil.INITIAL_COMPANY_NUM);
        if (ObjectUtils.isEmpty(config)) {
            enterprise.setChildNum(100);
        } else {
            enterprise.setChildNum(Integer.valueOf(config.getConfigValue()));
        }
        // 获取详细地址
        String address = regionService.assembleFullAddress(rq.getRegionid(), rq.getStreet());
        enterprise
                // 设置默认管理员为添加的人员
                .setAdmin(userInfo.getName())
                // 设置母公司id
                .setPid(rq.getId())
                .setAddress(address)
                // 设置状态及时间
                .setDeleted(EnumNewCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumNewCommon.IsActive.is_active.getKey())
                .setCreateTime(new Date())
                .setOperateId(userInfo.getId());
        // 从redis获取企业编号
        String enterpriseNoStr = jedisService.get("auto_enterprise_no");
        log.info("enterpriseNoStr={}", enterpriseNoStr);
        Integer enterpriseNo;
        if (ObjectUtils.isEmpty(enterpriseNoStr)) {
            jedisService.set("auto_enterprise_no", "1", 0);
            enterpriseNo = 1;
            log.info("enterpriseNo={},重新在redis缓冲放入企业自增序列", enterpriseNo);
        } else {
            enterpriseNo = Integer.valueOf(enterpriseNoStr);
        }
        // 根据是否有母公司 设置企业编号
        if (ObjectUtils.isEmpty(rq.getId()) || rq.getId() == 0) {
            enterprise.setEnterpriseNo(getCode(enterpriseNo) + "0000");
        } else {
            enterprise.setEnterpriseNo(getCode(enterpriseNo) + pEterprise.getEnterpriseNo().substring(0, 4));
        }
        // 编号序号自增
        jedisService.set("auto_enterprise_no", (enterpriseNo + 1) + "", 0);

        if (enterpriseMapper.insert(enterprise) != 1) {
            log.error("新增企业信息失败，类是：{}，方法是：{}", "AuthEnterpriseServiceImpl", "add");
            return ResponseResult.buildResponseResult(ResCodeEnum.ADD_FAILED);
        }
        // 插入附件信息
        insertFile(enterprise.getId(), rq.getLogosList(), rq.getEnclosuresList(), rq.getPermitsList(), rq.getCertificatesList(), userInfo);
        // 判断企业管理员手机是否注册：注册则不信增用户
        AuthPlatformUser admin = insertAdmin(rq, enterprise);
        // 插入管理员与角色关联信息
        insertAdminRole(userInfo, enterprise, admin);
        // 插入管理员与企业关联信息
        insertAdminEnterprise(userInfo, enterprise, admin);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    private static String getCode(Integer code) {
        if (code < 10) {
            return "100" + code;
        } else if (code < 100) {
            return "10" + code;
        } else if (code < 1000) {
            return "1" + code;
        } else {
            Integer code1 = (code / 1000) + 1;
            Integer code2 = code % 1000;
            return code1 + getCode(code2).substring(1, 4);
        }
    }

    /**
     * 插入管理员与企业关联信息
     */
    private void insertAdminEnterprise(AuthPlatformUserInfo userInfo, AuthEnterprise enterprise, AuthPlatformUser admin) {
        AuthPlatformUserEnterprise adminEnterprise = new AuthPlatformUserEnterprise()
                .setUserId(admin.getId())
                .setEnterpriseId(enterprise.getId())
                .setCreateUser(userInfo.getId())
                .setCreateTime(new Date())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey());
        userEnterpriseService.insert(adminEnterprise);
    }

    /**
     * 插入企业管理员角色信息
     */
    private void insertAdminRole(AuthPlatformUserInfo userInfo, AuthEnterprise enterprise, AuthPlatformUser admin) {
        AuthRole adminRole = roleService.selectOne(new EntityWrapper<>(new AuthRole()
                .setRoleType(EnumRoleType.ENTERPRISE_ADMIN.getCode())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())));
        AuthUserRole adminRoleRelation = new AuthUserRole()
                .setUserId(admin.getId())
                .setEnterpriseId(enterprise.getId())
                .setRoleType(adminRole.getRoleType())
                .setLevel(adminRole.getLevel())
                .setRoleId(adminRole.getId())
                .setCreateTime(new Date())
                .setCreateUser(userInfo.getId())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey());
        userRoleMapper.insert(adminRoleRelation);
    }

    /**
     * 判断企业管理员手机（中台）是否注册：注册则不新增用户，未注册默认新增一个用户
     */
    private AuthPlatformUser insertAdmin(AuthEnterpriseAddRQ rq, AuthEnterprise enterprise) {
        AuthPlatformUser admin;
        AuthPlatformUser validatePhone = userService.selectOne(new AuthPlatformUser()
                .setPhone(rq.getLinkman())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()));
        if (!ObjectUtils.isEmpty(validatePhone)) {
            log.info("该用户已经是其他企业的管理员了,现在开始管理的公司+1！类是：{}，方法是：{}", "AuthEnterpriseServiceImpl", "add");
            admin = validatePhone;
            // 如果用户原来是后台用户--则把用户设置成为中后台用户
            if (admin.getUserType().equals(UserType.BACKGROUND_USER.getKey())) {
                admin.setUserType(UserType.MIDDLE_BACKGROUND_USER.getKey());
            }
            userService.updateById(admin);
        } else {
            // 新注册一个用户
            admin = new AuthPlatformUser()
                    .setNickname(rq.getNickname())
                    .setName(rq.getLinkman())
                    .setUsername(rq.getLinkman())
                    .setPhone(rq.getLinkman())
                    .setPassword(BCryptPassword.encode("123456"))
                    .setActiveType(EnumEnterpriseUser.ActiveType.is_active.getKey())
                    .setUserType(UserType.MIDDLE_USER.getKey())
                    .setCurrentEnterpriseId(enterprise.getId())
                    .setCreateTime(new Date())
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setStatus(EnumCommon.IsActive.is_active.getKey());
            userService.insert(admin);
        }
        return admin;
    }

    /**
     * 更新企业信息
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> updateEnterprise(AuthEnterpriseUpdateRQ rq, AuthPlatformUserInfo userInfo) {
        AuthEnterprise enterprise = enterpriseMapper.selectOne(new AuthEnterprise().setId(rq.getId()));
        if (ObjectUtils.isEmpty(enterprise)) {
            log.error("企业信息不存在，类是：{}，方法是：{}", "AuthEnterpriseServiceImpl", "updateEnterprise");
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        // 更新企业基本信息
        org.springframework.beans.BeanUtils.copyProperties(rq, enterprise);
        enterprise.setUpdateTime(new Date());

        if (enterpriseMapper.updateById(enterprise) != 1) {
            log.error("更新企业信息失败，类是：{}，方法是：{}", "AuthEnterpriseServiceImpl", "updateEnterprise");
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        // 更新企业以前的附件信息
        commonFileService.update(new AuthCommonFile().setStatus(EnumNewCommon.IsActive.not_active.getKey())
                        .setDeleted(EnumCommon.IsDeleted.is_Delete.getKey()),
                new EntityWrapper<>(new AuthCommonFile().setEnterprisesId(rq.getId())
                        .setStatus(EnumNewCommon.IsActive.is_active.getKey())
                        .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())));

        List<FileRQ> logoList = rq.getLogosList();
        List<FileRQ> enclosuresList = rq.getEnclosuresList();
        List<FileRQ> permitsList = rq.getPermitsList();
        List<FileRQ> certificatesList = rq.getCertificatesList();
        // 插入附件信息
        insertFile(rq.getId(), logoList, enclosuresList, permitsList, certificatesList, userInfo);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 插入附件信息
     *
     * @param enterprisesId    企业id
     * @param logosList        logo信息
     * @param enclosuresList   营业执照
     * @param permitsList      开户许可证
     * @param certificatesList 企业认证许可
     */
    private ResponseResult<ResCodeEnum> insertFile(Integer enterprisesId,
                                                   List<FileRQ> logosList,
                                                   List<FileRQ> enclosuresList,
                                                   List<FileRQ> permitsList,
                                                   List<FileRQ> certificatesList,
                                                   AuthPlatformUserInfo userInfo) {
        // 添加附件信息
        ArrayList<FileRQ> fielList = Lists.newArrayList();
        // 营业执照
        fielList.addAll(enclosuresList);
        // 企业授权书
        fielList.addAll(certificatesList);
        // 开户许可证
        if (!CollectionUtils.isEmpty(permitsList)) {
            fielList.addAll(permitsList);
        }
        // logo
        if (!CollectionUtils.isEmpty(logosList)) {
            fielList.addAll(logosList);
        }

        List<AuthCommonFile> commonFiles = Lists.newArrayList();
        for (FileRQ fileRQ : fielList) {
            commonFiles.add(new AuthCommonFile().setName(fileRQ.getFileName())
                    .setUrl(fileRQ.getFileUrl())
                    .setType(fileRQ.getType())
                    .setCreateId(userInfo.getId())
                    .setEnterprisesId(enterprisesId)
                    .setStatus(EnumNewCommon.IsActive.is_active.getKey())
                    .setDeleted(EnumNewCommon.IsDeleted.not_Delete.getKey())
                    .setCreateTime(new Date()));
        }
        commonFileService.insertBatch(commonFiles);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 逻辑删除企业 修改删除状态 和 删除时间
     * 管理员只能删除 子公司
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult delEnterprise(AuthEnterpriseDelRQ rq, AuthPlatformUserInfo userInfo) {
        AuthEnterprise enterprise = enterpriseMapper.selectOne(new AuthEnterprise().setId(rq.getId()));
        if (ObjectUtils.isEmpty(enterprise)) {
            log.error("企业信息查询失败，类是：{}，方法是：{}", "AuthEnterpriseServiceImpl", "delEnterprise");
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        if (userInfo.getOrgId().equals(rq.getId())) {
            log.error("企业管理员不可删除自己管理的母公司，只可删除子公司，类是：{}，方法是：{}", "AuthEnterpriseServiceImpl", "delEnterprise");
            return ResponseResult.buildResponseResult(ResCodeEnum.ONLY_SUB_ENTERPRISE);
        }
        // 更新企业状态
        enterprise.setDeleted(EnumNewCommon.IsDeleted.is_Delete.getKey())
                .setDeletedTime(new Date())
                .setStatus(EnumCommon.IsActive.not_active.getKey());
        if (enterpriseMapper.updateById(enterprise) != 1) {
            log.error("更新企业信息失败，类是：{}，方法是：{}", "AuthEnterpriseServiceImpl", "delEnterprise");
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        // 更新企业附件信息
        commonFileService.update(new AuthCommonFile().setDeleted(EnumNewCommon.IsDeleted.is_Delete.getKey()),
                new EntityWrapper<>(new AuthCommonFile()
                        .setEnterprisesId(rq.getId())));
        // 更新企业用户中间表
        userEnterpriseService.update(new AuthPlatformUserEnterprise().setDeleted(EnumNewCommon.IsDeleted.is_Delete.getKey())
                , new EntityWrapper<>(new AuthPlatformUserEnterprise()
                        .setEnterpriseId(rq.getId())));
        // 更新企业角色中间表
        enterpriseRoleService.update(new AuthEnterpriseRole().setDeleted(EnumNewCommon.IsDeleted.is_Delete.getKey())
                , new EntityWrapper<>(new AuthEnterpriseRole()
                        .setEnterpriseId(rq.getId())));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 根据条件查询企业 优先精确匹配  被删除的子公司也会查询出来展示
     * 多次查询数据库 手动分页
     * 只能看到当前管理员的企业 及其子企业
     */
    @Override
    public ResponseResult<List<AuthEnterpriseListDTO>> getByConditional(AuthEnterpriseGetRQ rq, AuthPlatformUserInfo userInfo, Page page) {
        Integer userId = userInfo.getId();
        Integer orgId = userInfo.getOrgId();
        // 查询当前用户所在的企业
        AuthEnterprise enterprise = enterpriseMapper.selectOne(new AuthEnterprise().setId(orgId));
        if (ObjectUtils.isEmpty(enterprise)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        // 判断是否管理员  如果是管理员查询：企业及子企业
        // 不是管理员：只能看到所在的所有企业
        List<AuthEnterprise> list = Lists.newArrayList();
        Boolean manager = userService.isManager(orgId, userId, EnumRoleType.ENTERPRISE_ADMIN.getCode());
        if (manager) {
            List<AuthEnterprise> allEnterprises = enterpriseMapper.selectList(new EntityWrapper<>(new AuthEnterprise()
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setStatus(EnumCommon.IsActive.is_active.getKey()))
                    .orderBy("create_time", false));
            // 查询所有符合条件的企业
            getSubEnterpriseFlat(enterprise, list, allEnterprises);
        } else {
            // 只查询用户所在企业
            List<AuthPlatformUserEnterprise> userEnterpriseList = userEnterpriseService.selectList(new EntityWrapper<>(new AuthPlatformUserEnterprise()
                    .setUserId(userId)
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setStatus(EnumCommon.IsActive.is_active.getKey())));
            list = enterpriseMapper.selectList(new EntityWrapper<>(new AuthEnterprise()
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setStatus(EnumCommon.IsActive.is_active.getKey()))
                    .in("id", userEnterpriseList.stream().map(a -> a.getEnterpriseId()).collect(Collectors.toList()))
                    .orderBy("create_time", false));
        }
        /**----------------------根据条件过滤------------------------*/
        // 模糊查询
        if (!StringUtils.isEmpty(rq.getName())) {
            list = list.stream().filter(a -> a.getName().contains(rq.getName())).collect(Collectors.toList());
        }
        // 判断状态
        if (!ObjectUtils.isEmpty(rq.getStatus())) {
            list = list.stream().filter(a -> a.getStatus().equals(rq.getStatus())).collect(Collectors.toList());
        }
        // 判断时间
        if ((!ObjectUtils.isEmpty(rq.getStart()) && !ObjectUtils.isEmpty(rq.getEnd()))) {
            list = list.stream().filter(a -> a.getCreateTime().compareTo(rq.getStart()) >= 0
                    && a.getCreateTime().compareTo(rq.getEnd()) <= 0).collect(Collectors.toList());
        }
        /**----------------------拼装返回结果------------------------*/
        Map<Integer, String> nameMap = list.stream().collect(Collectors.toMap(AuthEnterprise::getId, AuthEnterprise::getName));
        // 返回结果list
        List<AuthEnterpriseListDTO> resultList = Lists.newArrayList();
        // 优化区域查询的map
        Map<Integer, Map<String, Object>> regionsMap = Maps.newHashMap();
        for (AuthEnterprise e1 : list) {
            AuthEnterpriseListDTO enterpriseListDTO = BeanUtils.copyProperties(e1, AuthEnterpriseListDTO.class);
            // 查询企业管理员
            List<AuthPlatformUser> managers = getEnterpriseManagers(e1.getId());
            enterpriseListDTO.setManagers(BeanUtils.assemble(MemberDto.class, managers));
            // 查询区域
            enterpriseListDTO.setAddress(getRegion(e1, regionsMap));
            // 上级公司名字
            if (!ObjectUtils.isEmpty(e1.getPid()) && !e1.getPid().equals(0)) {
                enterpriseListDTO.setPname(nameMap.get(e1.getPid()));
            }
            resultList.add(enterpriseListDTO);
        }
        /**----------------------设置分页数据------------------------*/
        int size = resultList.size();
        page.setTotalRecords(size);
        // 总页数 和 pagesize 取余
        int remainder = size % page.getPageSize();
        page.setTotalPage(remainder == 0 ? size / page.getPageSize() : size / page.getPageSize() + 1);
        // 如果每页显示数 大于结果的总数则需要分页
        List<AuthEnterpriseListDTO> subList = null;
        if (page.getPageSize() < resultList.size()) {
            int start = (page.getCurrentPage() - 1) * page.getPageSize();
            int end = (page.getCurrentPage() * page.getPageSize()) > size ? size : (page.getCurrentPage() * page.getPageSize());
            subList = resultList.subList(start, end);
        } else {
            subList = resultList;
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, subList, page);
    }

    /**
     * 根据条件查询企业 （后台使用）
     */
    @Override
    public ResponseResult<List<AuthEnterpriseListDTO>> getAllByConditional(AuthEnterpriseAllGetRQ rq, Page page) {
        Pagination pagination = PageUtils.transFromPage(page);
        // 构建条件
        EntityWrapper<AuthEnterprise> wrapper = new EntityWrapper<>();
        // 只查询未删除的企业
        wrapper.eq("deleted", EnumCommon.IsDeleted.not_Delete.getKey());
        if (!ObjectUtils.isEmpty(rq)) {
            // 根据id查询
            if (!StringUtils.isEmpty(rq.getName())) {
                wrapper.like("name", rq.getName());
            }
            // 根据创建时间
            if (!ObjectUtils.isEmpty(rq) && !ObjectUtils.isEmpty(rq.getStart()) && !ObjectUtils.isEmpty(rq.getEnd())) {
                wrapper.between("create_time", rq.getStart(), rq.getEnd());
            }
        }
        wrapper.orderBy("create_time", false);
        // 查询所有匹配的企业
        List<AuthEnterprise> enterprises = enterpriseMapper.selectPage(pagination, wrapper);
        if (CollectionUtils.isEmpty(enterprises)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST, Lists.newArrayList(), PageUtils.transToPage(pagination));
        }
        // 返回结果list
        List<AuthEnterpriseListDTO> resultList = Lists.newArrayList();

        List<AuthEnterprise> allEnterprises = enterpriseMapper.selectList(new EntityWrapper<>(new AuthEnterprise()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())));
        Map<Integer, String> nameMap = allEnterprises.stream().collect(Collectors.toMap(a -> a.getId(), b -> b.getName()));
        // 优化区域查询的map
        Map<Integer, Map<String, Object>> regionsMap = Maps.newHashMap();

        for (AuthEnterprise e1 : enterprises) {
            AuthEnterpriseListDTO enterpriseListDTO = BeanUtils.copyProperties(e1, AuthEnterpriseListDTO.class);
            // 查询企业管理员
            List<AuthPlatformUser> managers = getEnterpriseManagers(e1.getId());
            enterpriseListDTO.setManagers(BeanUtils.assemble(MemberDto.class, managers));
            // 查询区域
            enterpriseListDTO.setAddress(getRegion(e1, regionsMap));
            // 上级公司名字
            if (!ObjectUtils.isEmpty(e1.getPid())) {
                enterpriseListDTO.setPname(nameMap.get(e1.getPid()));
            }
            resultList.add(enterpriseListDTO);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, resultList, PageUtils.transToPage(pagination));
    }

    /**
     * 查询企业的子公司（扁平结构）
     */
    private List<AuthEnterprise> getSubEnterpriseFlat(AuthEnterprise enterprise, List<AuthEnterprise> list, List<AuthEnterprise> allEnterprises) {
        if (!list.contains(enterprise)) {
            list.add(enterprise);
        }
        for (AuthEnterprise e1 : allEnterprises) {
            if (enterprise.getId().equals(e1.getPid())) {
                if (!list.contains(e1)) {
                    list.add(e1);
                }
                getSubEnterpriseFlat(e1, list, allEnterprises);
            }
        }
        return list;
    }

    /**
     * 查询企业地址
     *
     * @param enterprise 企业信息
     * @param map        多个企业查询时 将区县id作为key 缓存到map
     * @return 企业地址的String 省市区+详细街道地址
     */
    @Override
    public String getRegion(AuthEnterprise enterprise, Map<Integer, Map<String, Object>> map) {
        // 如果map中有  则从map中取
        Map<String, Object> regions = null;
        if (enterprise.getRegionid() != null
                && !ObjectUtils.isEmpty(map.get(enterprise.getRegionid()))) {
            regions = map.get(enterprise.getRegionid());
        } else {
            ResponseResult<Map<String, Object>> result = commonRegionService.findAllRegionById(enterprise.getRegionid());
            if (result.getCode().equals(ResCodeEnum.FAILED.code)) {
                return "";
            }
            regions = result.getObject();
            Map<String, Object> resgionsMap = Maps.newHashMap();
            resgionsMap.put("province", regions.get("province"));
            resgionsMap.put("city", regions.get("city"));
            resgionsMap.put("county", regions.get("county"));
            map.put(enterprise.getRegionid(), resgionsMap);
        }
        CommonRegion province = (CommonRegion) regions.get("province");
        CommonRegion city = (CommonRegion) regions.get("city");
        CommonRegion county = (CommonRegion) regions.get("county");
        StringBuffer stringBuffer = new StringBuffer().append(province.getDistrict()).append(city.getDistrict())
                .append(county.getDistrict()).append(enterprise.getStreet());
        return stringBuffer.toString();
    }

    /**
     * 查询企业的管理员们
     */
    private List<AuthPlatformUser> getEnterpriseManagers(Integer enterpriseId) {
        List<AuthUserRole> userRoles = userRoleMapper.selectList(new EntityWrapper<>(new AuthUserRole()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())
                .setEnterpriseId(enterpriseId)
                .setRoleType(EnumRoleType.ENTERPRISE_ADMIN.getCode())));
        if (CollectionUtils.isEmpty(userRoles)) {
            return Lists.newArrayList();
        }
        List<Integer> ids = userRoles.stream().map(AuthUserRole::getUserId).collect(Collectors.toList());
        return userService.selectList(new EntityWrapper<>(new AuthPlatformUser()
                .setDeleted(EnumNewCommon.IsDeleted.not_Delete.getKey()))
                .in("id", ids));
    }

    /**
     * 查询企业详情
     */
    @Override
    public ResponseResult<AuthEnterpriseDetailDTO> getEnterpriseDetail(Integer enterpriseId) {
        AuthEnterprise enterprise = enterpriseMapper.selectOne(new AuthEnterprise().setId(enterpriseId));
        if (ObjectUtils.isEmpty(enterprise)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        AuthEnterpriseDetailDTO detailDTO = new AuthEnterpriseDetailDTO();
        // 公司基本信息
        org.springframework.beans.BeanUtils.copyProperties(enterprise, detailDTO);
        // 公司附件信息
        List<AuthCommonFile> commonFiles = commonFileService.selectList(new EntityWrapper<>(new AuthCommonFile()
                .setEnterprisesId(enterpriseId)
                .setStatus(EnumNewCommon.IsActive.is_active.getKey())));
        List<FileRQ> logoList = Lists.newArrayList();
        // 营业执照
        List<FileRQ> enclosuresList = Lists.newArrayList();
        // 开户许可
        List<FileRQ> permitsList = Lists.newArrayList();
        // 企业认证授权书
        List<FileRQ> certificatesList = Lists.newArrayList();
        for (AuthCommonFile commonFile : commonFiles) {
            switch (commonFile.getType()) {
                case 0:
                    enclosuresList.add(new FileRQ().setFileName(commonFile.getName())
                            .setFileUrl(commonFile.getUrl())
                            .setType(commonFile.getType()));
                    break;
                case 1:
                    permitsList.add(new FileRQ().setFileName(commonFile.getName())
                            .setFileUrl(commonFile.getUrl())
                            .setType(commonFile.getType()));
                    break;
                case 2:
                    certificatesList.add(new FileRQ().setFileName(commonFile.getName())
                            .setFileUrl(commonFile.getUrl())
                            .setType(commonFile.getType()));
                    break;
                case 3:
                    logoList.add(new FileRQ().setFileName(commonFile.getName())
                            .setFileUrl(commonFile.getUrl())
                            .setType(commonFile.getType()));
                    break;
                default:
            }
        }
        detailDTO.setLogo(logoList)
                .setEnclosuresList(enclosuresList)
                .setPermitsList(permitsList)
                .setCertificatesList(certificatesList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, detailDTO);
    }

    /**
     * 根据企业ids查询企业基础信息详情
     */
    @Override
    public ResponseResult<List<AuthEnterpriseFeignDetailDTO>> getEnterpriseMoreDetail(List<Integer> orgIds) {
        if (CollectionUtils.isEmpty(orgIds)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>(0));
        }
        List<AuthEnterprise> enterpriseList = this.selectList(new EntityWrapper<>(new AuthEnterprise()
                .setDeleted(Status.FALSE.getKey())
                .setStatus(Status.TRUE.getKey()))
                .in("id", orgIds));
        List<AuthEnterpriseFeignDetailDTO> feignDetailDTOS = BeanUtils.assemble(AuthEnterpriseFeignDetailDTO.class, enterpriseList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, feignDetailDTOS);
    }

    @Override
    public ResponseResult<List<AuthEnterpriseAllDTO>> getAllEnterprise() {
        List<AuthEnterprise> enterprises = enterpriseMapper.selectList(new EntityWrapper<>(new AuthEnterprise().setDeleted(EnumNewCommon.IsDeleted.not_Delete.getKey())));
        List<AuthEnterpriseAllDTO> list = BeanUtils.assemble(AuthEnterpriseAllDTO.class, enterprises);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
    }

    /**
     * 查询所有企业及其子公司（树形结构）
     */
    @Override
    public ResponseResult<List<AuthEnterpriseTreeDTO>> getEnterpriseTree() {
        List<AuthEnterprise> enterprises = enterpriseMapper.selectList(new EntityWrapper<>(new AuthEnterprise().setDeleted(EnumNewCommon.IsDeleted.not_Delete.getKey())));
        List<AuthEnterpriseTreeDTO> allEnterpriseDTOList = Lists.newArrayList();
        for (AuthEnterprise enterprise : enterprises) {
            allEnterpriseDTOList.add(new AuthEnterpriseTreeDTO().setValue(enterprise.getId()).setLabel(enterprise.getName()).setPid(enterprise.getPid()));
        }
        // 一级企业
        List<AuthEnterprise> ones = enterprises.stream().filter(a -> ObjectUtils.isEmpty(a.getPid())).collect(Collectors.toList());
        List<AuthEnterpriseTreeDTO> list = Lists.newArrayList();
        for (AuthEnterprise one : ones) {
            list.add(new AuthEnterpriseTreeDTO().setValue(one.getId()).setLabel(one.getName()).setPid(one.getPid()));
        }
        for (AuthEnterpriseTreeDTO oneDTO : list) {
            build(oneDTO, allEnterpriseDTOList);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
    }

    private void build(AuthEnterpriseTreeDTO treeDTO, List<AuthEnterpriseTreeDTO> allEnterpriseDTOList) {
        for (AuthEnterpriseTreeDTO dto : allEnterpriseDTOList) {
            if (dto.getPid() != null && dto.getPid().equals(treeDTO.getValue())) {
                if (treeDTO.getChildren() == null) {
                    treeDTO.setChildren(Lists.newArrayList());
                }
                treeDTO.getChildren().add(dto);
                build(dto, allEnterpriseDTOList);
            }
        }
    }

    @Override
    public ResponseResult<AuthEnterpriseTreeDTO> getEnterpriseTreeByUser(AuthPlatformUserInfo userInfo) {
        if (ObjectUtils.isEmpty(userInfo.getOrgId())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED);
        }
        // 当前用户企业
        AuthEnterprise enterprise = enterpriseMapper.selectOne(new AuthEnterprise().setId(userInfo.getOrgId()).setDeleted(EnumNewCommon.IsDeleted.not_Delete.getKey()));
        if (ObjectUtils.isEmpty(enterprise)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        AuthEnterpriseTreeDTO treeDTO = new AuthEnterpriseTreeDTO().setValue(enterprise.getId()).setLabel(enterprise.getName()).setPid(enterprise.getPid());
        // 所有企业
        List<AuthEnterprise> enterprises = enterpriseMapper.selectList(new EntityWrapper<>(new AuthEnterprise().setDeleted(EnumNewCommon.IsDeleted.not_Delete.getKey())));
        List<AuthEnterpriseTreeDTO> allEnterpriseDTOList = Lists.newArrayList();
        for (AuthEnterprise e1 : enterprises) {
            allEnterpriseDTOList.add(new AuthEnterpriseTreeDTO().setValue(e1.getId()).setLabel(e1.getName()).setPid(e1.getPid()));
        }
        build(treeDTO, allEnterpriseDTOList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, treeDTO);
    }

    /**
     * 根据用户查询 用户企业 下拉列表使用
     */
    @Override
    public ResponseResult<List<AuthEnterpriseFlatDTO>> getEnterpriseFlatByUser(AuthPlatformUserInfo userInfo) {
        AuthEnterprise enterprise = enterpriseMapper.selectOne(new AuthEnterprise()
                .setId(userInfo.getOrgId())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()));
        if (ObjectUtils.isEmpty(enterprise)) {
            log.error("查询企业及其子企业失败，企业不存在，类是：{}，方法是：{}", "AuthEnterpriseServiceImpl", "getEnterpriseFlatByUser");
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        List<AuthEnterprise> allEnterprises = enterpriseMapper.selectList(new EntityWrapper<>(new AuthEnterprise()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())));
        List<AuthEnterprise> list = Lists.newArrayList();
        List<AuthEnterpriseFlatDTO> result = Lists.newArrayList();
        this.getSubEnterpriseFlat(enterprise, list, allEnterprises);
        for (AuthEnterprise e : list) {
            result.add(new AuthEnterpriseFlatDTO()
                    .setValue(e.getId())
                    .setLabel(e.getName())
                    .setPid(e.getPid()));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }

    /**
     * 根据用户 查询企业及其父子企业
     */
    @Override
    public ResponseResult<List<AuthEnterpriseFlatDTO>> getParentSubEnterpriseFlat(AuthPlatformUserInfo userInfo) {
        AuthEnterprise parent = getAncestor(userInfo.getOrgId());
        return getEnterpriseFlatByUser(userInfo.setOrgId(parent.getId()));
    }

    /**
     * 子企业找祖宗
     */
    @Override
    public AuthEnterprise getAncestor(Integer enterpriseId) {
        AuthEnterprise enterprise = this.selectOne(new EntityWrapper<>(new AuthEnterprise()
                .setId(enterpriseId)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())));
        if (ObjectUtils.isEmpty(enterprise)) {
            log.error("找不到对应企业 {}：{}", "AuthEnterpriseServiceImpl", "getParent");
            throw new BusinessException(ResCodeEnum.ENTERPRISE_NOT_EXIST, ExceptionMessageEnum.ENTERPRISE_NOT_EXIST);
        }
        if (ObjectUtils.isEmpty(enterprise.getPid()) || enterprise.getPid() == 0) {
            return enterprise;
        }
        return getAncestor(enterprise.getPid());
    }

    /**
     * 查询企业历史详情(后台)
     */
    @Override
    public ResponseResult<AuthEnterpriseHistoryDTO> getEnterpriseHistory(Integer enterpriseId, AuthPlatformUserInfo userInfo) {
        AuthEnterprise enterprise = selectOne(new EntityWrapper<>(new AuthEnterprise().setId(enterpriseId)));
        // 基础数据
        AuthEnterpriseHistoryDTO historyDTO = BeanUtils.copyProperties(enterprise, AuthEnterpriseHistoryDTO.class);
        // 部门
        List<Departments> departments = departmentsService.getByEnterpriseId(enterpriseId, userInfo).getObject();
        // 职位
        ArrayList<ZPost> posts = Lists.newArrayList();
        for (Departments department : departments) {
            List<ZPost> postList = postService.getByDepartment(department.getId()).getObject();
            if (!CollectionUtils.isEmpty(postList)) {
                posts.addAll(postList);
            }
        }
        // 成员
        List<AuthPlatformUser> user = userEnterpriseService.getEnterpriseUser(enterpriseId);
        List<MemberDto> memberDtos = BeanUtils.assemble(MemberDto.class, user);
        // 附件信息
        // 公司附件信息
        List<AuthCommonFile> commonFiles = commonFileService.selectList(new EntityWrapper<>(new AuthCommonFile()
                .setEnterprisesId(enterpriseId)
                .setStatus(EnumNewCommon.IsActive.is_active.getKey())));
        FileRQ logo = new FileRQ();
        // 营业执照
        List<FileRQ> enclosuresList = Lists.newArrayList();
        // 开户许可
        List<FileRQ> permitsList = Lists.newArrayList();
        // 企业认证授权书
        List<FileRQ> certificatesList = Lists.newArrayList();
        for (AuthCommonFile commonFile : commonFiles) {
            switch (commonFile.getType()) {
                case 0:
                    enclosuresList.add(new FileRQ().setFileName(commonFile.getName())
                            .setFileUrl(commonFile.getUrl())
                            .setType(commonFile.getType()));
                    break;
                case 1:
                    permitsList.add(new FileRQ().setFileName(commonFile.getName())
                            .setFileUrl(commonFile.getUrl())
                            .setType(commonFile.getType()));
                    break;
                case 2:
                    certificatesList.add(new FileRQ().setFileName(commonFile.getName())
                            .setFileUrl(commonFile.getUrl())
                            .setType(commonFile.getType()));
                    break;
                case 3:
                    logo = new FileRQ().setFileName(commonFile.getName())
                            .setFileUrl(commonFile.getUrl())
                            .setType(commonFile.getType());
                    break;
                default:
            }
        }

        Industry industry = industryService.selectById(enterprise.getIndustry());
        historyDTO.setAddress(getRegion(enterprise, Maps.newHashMap()))
                .setDepartments(BeanUtils.assemble(DepartmentDTO.class, departments))
                .setMembers(memberDtos)
                .setPosts(BeanUtils.assemble(ZPostDTO.class, posts))
                .setIndustry(industry == null ? "" : industry.getIndustry())
                .setCertificatesList(certificatesList)
                .setEnclosuresList(enclosuresList)
                .setPermitsList(permitsList);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, historyDTO);
    }

    /**
     * 根据当前企业查询所有上级公司id
     */
    @Override
    public List<Integer> getParentEnterpriseIds(Integer enterpriseId) {
        AuthEnterprise enterprise = enterpriseMapper.selectOne(new AuthEnterprise()
                .setId(enterpriseId)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()));
        ArrayList<Integer> list = Lists.newArrayList();
        return getParents(enterprise, list);
    }

    /**
     * @param list :
     * @notes: 批量插入
     * @Author: junyang.li
     * @Date: 15:44 2019/6/6
     * @return: void
     */
    @Override
    public void insertAll(List<AuthEnterprise> list) {
        enterpriseMapper.insertAll(list);
    }

    private List<Integer> getParents(AuthEnterprise enterprise, List<Integer> list) {
        if (!ObjectUtils.isEmpty(enterprise.getPid())) {
            AuthEnterprise parent = enterpriseMapper.selectOne(new AuthEnterprise().setId(enterprise.getPid()));
            list.add(parent.getId());
            getParents(parent, list);
        }
        return list;
    }

    /**
     * 根据当前企业id查询
     */
    @Override
    public ResponseResult<AuthEnterpriseDTO> getParentEnterpriseId(Integer enterpriseId) {
        AuthEnterpriseDTO authEnterpriseDTO = new AuthEnterpriseDTO();
        AuthEnterprise enterprise = enterpriseMapper.selectOne(new AuthEnterprise()
                .setId(enterpriseId));
        if (Objects.isNull(enterprise)) {
            return null;
        }
        BeanUtils.copyProperties(enterprise, authEnterpriseDTO);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, authEnterpriseDTO);

    }

    /**
     * 根据用户id查询-用户所在企业及子企业
     */
    @Override
    public ResponseResult<List<EnterpriseListDTO>> getUserEnterprises(Integer userId) {
        List<AuthPlatformUserEnterprise> userEnterpriseList = userEnterpriseService.selectList(new EntityWrapper<>(new AuthPlatformUserEnterprise()
                .setUserId(userId)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())));
        List<AuthEnterprise> allEnterprises = enterpriseMapper.selectList(new EntityWrapper<>(new AuthEnterprise()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())));
        List<Integer> orgIds = userEnterpriseList.stream().map(AuthPlatformUserEnterprise::getEnterpriseId).collect(Collectors.toList());
        List<AuthEnterprise> enterprises = allEnterprises.stream().filter(e -> orgIds.contains(e.getId())).collect(Collectors.toList());
        List<AuthEnterprise> list = Lists.newArrayList();
        for (AuthEnterprise enterprise : enterprises) {
            // 查询所有符合条件的企业
            getSubEnterpriseFlat(enterprise, list, allEnterprises);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, BeanUtils.assemble(EnterpriseListDTO.class, list));
    }

    /**
     * 根据企业id查询-集团下的所有用户
     */
    @Override
    public ResponseResult<List<AuthPlatformUserDto>> getEnterpriseUsers(Integer userId) {
        List<AuthPlatformUserDto> enterpriseUsers = null;
        //获取当前用户所在企业及子企业
        List<EnterpriseListDTO> enterpriseList = getUserEnterprises(userId).getObject();
        if (!CollectionUtils.isEmpty(enterpriseList)) {
            List<Integer> enterpriseIds = new ArrayList<>();
            enterpriseList.forEach(enterprise -> {
                enterpriseIds.add(enterprise.getId());
            });
            enterpriseUsers = authPlatformUserMapper.findEnterpriseUsers(enterpriseIds);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, enterpriseUsers);
    }

    /*------------------------------以前的接口-------------------------------*/

    @Override
    public ResponseResult<List<EnterpriseInfoDTO>> listRegisteredEnterpriseInfo(String enterpriseName) {
        List<AuthEnterprise> enterprises = enterpriseMapper.selectList(new EntityWrapper<>(new AuthEnterprise().setName(enterpriseName)));
        List<EnterpriseDetailDTO> enterpriseDetailList = BeanUtils.assemble(EnterpriseDetailDTO.class, enterprises);
        if (org.apache.commons.collections.CollectionUtils.isEmpty(enterpriseDetailList)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        if (org.apache.commons.collections.CollectionUtils.size(enterpriseDetailList) >= 2) {
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
        if (org.apache.commons.lang.StringUtils.isEmpty(name)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        String companyName = name.replaceAll("\\s*", "");
        List<AuthEnterprise> exist = this.selectList(new EntityWrapper<AuthEnterprise>()
                .eq("name", companyName)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        // 查询企业审核表该企业是否有已通过的记录
        List<Integer> passedType = Arrays.asList(1, 3, 4, 5);
        List<EnterprisesCheck> passed = enterprisesCheckService.selectList(new EntityWrapper<EnterprisesCheck>()
                .in("type", passedType)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey())
                .eq("name", name));
        // 校验企业是否已注册
        if (org.apache.commons.collections.CollectionUtils.isNotEmpty(exist) || org.apache.commons.collections.CollectionUtils.isNotEmpty(passed)) {
            log.info("企业已注册");
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_EXIST, EnumEnterpriseCheck.checkCompany.EXIST.getKey());
        }
        List<EnterprisesCheck> inAudit = enterprisesCheckService.selectList(new EntityWrapper<EnterprisesCheck>()
                .eq("name", companyName)
                .eq("type", EnumEnterpriseCheck.CheckType.IN_AUDIT_REGISTER.getKey())

                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        // 校验企业是否在审核中
        if (!org.apache.commons.collections.CollectionUtils.isEmpty(inAudit)) {
            log.info("企业正在审核中");
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_IN_AUDIT, EnumEnterpriseCheck.checkCompany.IN_AUDIT.getKey());
        }
        List<EnterprisesCheck> refused = enterprisesCheckService.selectList(new EntityWrapper<EnterprisesCheck>()
                .eq("name", companyName)
                .eq("type", EnumEnterpriseCheck.CheckType.REFUSED_REGISTER.getKey())
//                .eq("create_id",userInfo.getId())
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        if (refused.size() > 1) {
            log.info("企业正在审核中");
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_IN_AUDIT, EnumEnterpriseCheck.checkCompany.IN_AUDIT.getKey());

        }
        if (org.apache.commons.collections.CollectionUtils.isNotEmpty(refused) && !refused.get(0).getCreateId().equals(userInfo.getId())) {
            log.info("企业正在审核中");
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_IN_AUDIT, EnumEnterpriseCheck.checkCompany.IN_AUDIT.getKey());
        }
        // 企业可注册
        log.info("企业可注册");
        return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_REGISTERED_IS_OK, EnumEnterpriseCheck.checkCompany.NON_EXIST.getKey());

    }

    /**
     * @notes 获取当前用户的企业信息
     **/
    @Override
    public ResponseResult<List<EnterpriseListDTO>> getAllEnterprise(AuthPlatformUserInfo userInfo) {
        //查询用户关联的企业
        List<AuthPlatformUserEnterprise> enterprisesUsers = userEnterpriseService.selectList(new EntityWrapper<>(new AuthPlatformUserEnterprise()
                .setUserId(userInfo.getId())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())));
        if (org.apache.commons.collections.CollectionUtils.isEmpty(enterprisesUsers)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, new ArrayList<>());
        }
        List<Integer> enterpriseIds = enterprisesUsers.stream().map(AuthPlatformUserEnterprise::getEnterpriseId).collect(Collectors.toList());
        return searchEnterpriseUser(enterpriseIds);
    }

    /**
     * @param enterpriseIds :
     * @notes: 根据企业id，查询企业基本情况，部门数， 成员数等
     */
    private ResponseResult<List<EnterpriseListDTO>> searchEnterpriseUser(List<Integer> enterpriseIds) {
        //查询企业
        List<AuthEnterprise> enterprises = enterpriseMapper.selectList(new EntityWrapper<AuthEnterprise>()
                .in("id", enterpriseIds));
        //企业部门统计
        List<EnterprisesCountDTO> countDeparts = departmentsMapper.countDepartment(enterpriseIds);
        Map<Integer, Integer> departmentCount = new SoftReference<>(new HashMap<Integer, Integer>(16)).get();
        if (!org.apache.commons.collections.CollectionUtils.isEmpty(countDeparts)) {
            countDeparts.forEach(obj -> departmentCount.put(obj.getOrgId(), obj.getCount()));
        }
        //企业人员统计
        List<EnterprisesCountDTO> enterprisesCountUsers = userEnterpriseMapper.countUserByOrgIds(enterpriseIds);
        Map<Integer, Integer> userCount = new SoftReference<>(new HashMap<Integer, Integer>(16)).get();
        if (!org.apache.commons.collections.CollectionUtils.isEmpty(enterprisesCountUsers)) {
            enterprisesCountUsers.forEach(obj -> userCount.put(obj.getOrgId(), obj.getCount()));
        }
        //遍历企业信息，添加公司人数、部门数
        List<EnterpriseListDTO> list = enterprises.stream().map(obj -> {
            Integer departmentNum = 0;
            if (!CollectionUtils.isEmpty(departmentCount)) {
                departmentNum = departmentCount.get(obj.getId());
            }
            Integer userNum = 0;
            if (!CollectionUtils.isEmpty(userCount)) {
                userNum = userCount.get(obj.getId());
            }
            return BeanUtils.copyProperties(obj, EnterpriseListDTO.class)
                    .setDepartmentNum(departmentNum)
                    .setUserNum(userNum);
        }).collect(Collectors.toList());
        /**-------------查询企业头像--------------------**/
        List<AuthCommonFile> heads = commonFileService.selectList(new EntityWrapper<>(new AuthCommonFile()
                .setType(EnumFileType.FileType.logo.getKey())
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey()))
                .in("enterprises_id", enterpriseIds));
        if (!CollectionUtils.isEmpty(heads)) {
            Map<Integer, AuthCommonFile> headMap = heads.stream().collect(Collectors.toMap(AuthCommonFile::getEnterprisesId, b -> b));
            for (EnterpriseListDTO dto : list) {
                // 根据企业id获取头像对象
                AuthCommonFile head = headMap.get(dto.getId());
                if (!ObjectUtils.isEmpty(head)) {
                    dto.setHead(head.getUrl());
                }
            }
        }
        /**---------------------end------------------------**/
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, list);
    }

    /**
     * 修改企业头像
     */
    @Override
    public ResponseResult<String> modifyEnterpriseHead(String head, AuthPlatformUserInfo userInfo) {
        AuthEnterprise enterprise = enterpriseMapper.selectOne(new AuthEnterprise().setId(userInfo.getOrgId()));
        if (ObjectUtils.isEmpty(enterprise)) {
            throw new BusinessException(ResCodeEnum.ENTERPRISE_NOT_EXIST, ExceptionMessageEnum.ENTERPRISE_NOT_EXIST);
        }
        AuthCommonFile commonFile = commonFileService.selectOne(new EntityWrapper<>(new AuthCommonFile()
                .setEnterprisesId(userInfo.getOrgId())
                .setType(EnumFileType.FileType.logo.getKey())));
        // 如果还没头像  则插入 否则修改
        if (ObjectUtils.isEmpty(commonFile)) {
            commonFileService.insert(new AuthCommonFile()
                    .setUrl(head)
                    .setName("企业" + userInfo.getOrgId() + "的头像")
                    .setEnterprisesId(userInfo.getOrgId())
                    .setCreateId(userInfo.getId())
                    .setCreateTime(new Date())
                    .setType(EnumFileType.FileType.logo.getKey())
                    .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                    .setStatus(EnumCommon.IsActive.is_active.getKey()));
        } else {
            // 修改则更新
            commonFileService.updateById(commonFile.setUrl(head));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 根据id查询企业
     */
    @Override
    public ResponseResult<EnterpriseDetailDTO> getById(Integer orgId) {
        AuthEnterprise enterprise = this.selectOne(new EntityWrapper<>(new AuthEnterprise().setId(orgId).setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())));
        EnterpriseDetailDTO detailDTO = BeanUtils.copyProperties(enterprise, EnterpriseDetailDTO.class);
        if (!ObjectUtils.isEmpty(enterprise.getIndustry())) {
            Industry enterpriseIndustry = industryService.selectById(enterprise.getIndustry());
            List<Industry> industryList = industryService.selectList(new EntityWrapper<>(new Industry()));
            IndustryDTO industryDTO = new IndustryDTO();
            for (Industry industry : industryList) {
                if (industry.getPid().equals(enterprise.getId())) {
                    industryDTO.setChildIndustry(BeanUtils.copyProperties(industry, IndustryInfo.class));
                }
                if (industry.getId().equals(enterpriseIndustry.getPid())) {
                    industryDTO.setParentIndustry(BeanUtils.copyProperties(industry, IndustryInfo.class));
                }
            }
            detailDTO.setIndustry(industryDTO);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, detailDTO);
    }

    /**
     * 模糊查询企业列表
     *
     * @param name 公司名称
     * @return 公司列表
     */
    @Override
    public ResponseResult<List<EnterpriseSearchDTO>> searchEnterpriseList(String name, Page page) {

        String companyName = name.replaceAll("\\s*", "");
        Pagination pagination = PageUtils.transFromPage(page);
        List<AuthEnterprise> list = enterpriseMapper.selectPage(pagination, new EntityWrapper<AuthEnterprise>()
                .like("name", companyName)
                .eq("status", EnumCommon.IsActive.is_active.getKey())
                .eq("deleted", EnumCommon.IsDeleted.not_Delete.getKey()));
        List<EnterpriseSearchDTO> dto = BeanUtils.assemble(EnterpriseSearchDTO.class, list);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto, PageUtils.transToPage(pagination));

    }

    /**
     * 根据企业id查询企业及子企业
     *
     * @param companyId
     * @return
     */
    @Override
    public ResponseResult<List<AuthEnterpriseFlatDTO>> getEnterpriseFlatByCompanyId(Integer companyId) {
        AuthEnterprise enterprise = enterpriseMapper.selectOne(new AuthEnterprise()
                .setId(companyId)
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey()));
        if (ObjectUtils.isEmpty(enterprise)) {
            log.error("查询企业及其子企业失败，企业不存在，类是：{}，方法是：{}", "AuthEnterpriseServiceImpl", "getEnterpriseFlatByUser");
            return ResponseResult.buildResponseResult(ResCodeEnum.ENTERPRISE_NOT_EXIST);
        }
        List<AuthEnterprise> allEnterprises = enterpriseMapper.selectList(new EntityWrapper<>(new AuthEnterprise()
                .setDeleted(EnumCommon.IsDeleted.not_Delete.getKey())
                .setStatus(EnumCommon.IsActive.is_active.getKey())));
        List<AuthEnterprise> list = Lists.newArrayList();
        List<AuthEnterpriseFlatDTO> result = Lists.newArrayList();
        this.getSubEnterpriseFlat(enterprise, list, allEnterprises);
        for (AuthEnterprise e : list) {
            result.add(new AuthEnterpriseFlatDTO()
                    .setValue(e.getId())
                    .setLabel(e.getName())
                    .setPid(e.getPid()));
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result);
    }


}

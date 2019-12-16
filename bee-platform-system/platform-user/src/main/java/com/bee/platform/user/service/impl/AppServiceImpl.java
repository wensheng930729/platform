package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.entity.UserInfo;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.user.dao.mapper.AppMapper;
import com.bee.platform.user.dao.mapper.EnterprisesAppsMapper;
import com.bee.platform.user.dao.mapper.EnterprisesUsersMapper;
import com.bee.platform.user.dto.AppDTO;
import com.bee.platform.user.entity.App;
import com.bee.platform.user.entity.EnterprisesApps;
import com.bee.platform.user.entity.EnterprisesUsers;
import com.bee.platform.user.entity.User;
import com.bee.platform.user.service.AppService;
import com.bee.platform.user.service.EnterprisesAppsService;
import com.bee.platform.user.service.EnterprisesUsersService;
import com.bee.platform.user.service.UsersService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import java.util.*;

/**
 * <p>
 * app 服务实现类
 * </p>
 *
 * @author chengke
 * @since 2019-03-18
 */
@Slf4j
@Service
public class AppServiceImpl extends ServiceImpl<AppMapper, App> implements AppService {

    @Autowired
    private EnterprisesAppsMapper enterprisesAppsMapper;
    @Autowired
    private EnterprisesAppsService enterprisesAppsService;
    @Autowired
    private EnterprisesUsersMapper enterprisesUsersMapper;
    @Autowired
    private EnterprisesUsersService enterprisesUsersService;
    @Autowired
    private UsersService usersService;


    /**
     * 根据企业Id查询用户的应用app
     *
     * @param userInfo
     * @return
     */
    @Override
    public List<AppDTO> getAppsListByOrgId(AuthPlatformUserInfo userInfo) {

        if (ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("未获得企业ID");
            throw new BusinessException(ResCodeEnum.NOT_FOUND_COMPANY_ID, ExceptionMessageEnum.ENTERPRISE_ID_NOT_EXIST);
        }
        int orgId = userInfo.getOrgId();
        // 根据企业id查询企业应用信息
        List<EnterprisesApps> lists = enterprisesAppsMapper.selectList(new EntityWrapper<EnterprisesApps>()
                .eq("org_id", orgId));
        List<AppDTO> apps = new ArrayList<>();
        for (EnterprisesApps relation : lists) {
            if (!ObjectUtils.isEmpty(relation.getAppId())) {
                App app = this.selectById(relation.getAppId());
                if (!ObjectUtils.isEmpty(app)) {
                    AppDTO appDTO = BeanUtils.copyProperties(app, AppDTO.class);
                    apps.add(appDTO);
                }
            }
        }
        return apps;
    }

    /**
     * 根据用户ID获取用户应用app
     *
     * @param userInfo
     * @param userId
     * @return
     */
    @Override
    public List<AppDTO> getAppsList(AuthPlatformUserInfo userInfo, int userId) {
        if (ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("未获得企业ID");
            throw new BusinessException(ResCodeEnum.NOT_FOUND_COMPANY_ID, ExceptionMessageEnum.ENTERPRISE_ID_NOT_EXIST);
        }
        int orgId = userInfo.getOrgId();
        // 根据企业id查询企业应用信息
        List<EnterprisesApps> lists = enterprisesAppsMapper.selectList(new EntityWrapper<EnterprisesApps>().eq("org_id", orgId));
        List<AppDTO> appDTOList = new ArrayList<>();
        for (EnterprisesApps relation : lists) {
            if (!ObjectUtils.isEmpty(relation.getAppId())) {
                int appId = relation.getAppId();
                App app = selectById(appId);
                AppDTO appDTO = BeanUtils.copyProperties(app, AppDTO.class);
                appDTO.setOrgId(orgId);
                appDTO.setUrl(relation.getUrl());
                // 根据用户id和企业id查询用户企业信息
                EnterprisesUsers enterprisesUsers = enterprisesUsersService.selectOne(new EntityWrapper<EnterprisesUsers>()
                        .eq("user_id", userId)
                        .eq("enterprise_id", orgId));
                if (!ObjectUtils.isEmpty(enterprisesUsers) && StringUtils.hasLength(enterprisesUsers.getAppIds())) {
                    String[] appIds = StringUtils.split(enterprisesUsers.getAppIds(), ",");
                    if (!ObjectUtils.isEmpty(appIds) && Arrays.asList(appIds).contains(String.valueOf(app.getId()))) {
                        appDTO.setStatus(1);
                    } else {
                        appDTO.setStatus(0);
                    }
                }
                appDTOList.add(appDTO);
            }

        }
        return appDTOList;
    }

    /**
     * 用户获取应用列表
     *
     * @param userInfo
     * @return
     */
    @Override
    public List<AppDTO> getAppListByUser(AuthPlatformUserInfo userInfo) {

        if (ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("未获得企业ID");
            throw new BusinessException(ResCodeEnum.NOT_FOUND_COMPANY_ID, ExceptionMessageEnum.ENTERPRISE_ID_NOT_EXIST);
        }
        if (ObjectUtils.isEmpty(userInfo.getUsername())) {
            log.error("未获得用户账号");
            throw new BusinessException(ResCodeEnum.NOT_FOUND_USERNAME, ExceptionMessageEnum.USERNAME_NOT_FOUND);
        }
        int orgId = userInfo.getOrgId();
        // 根据用户账号查询用户信息
        String userName = userInfo.getUsername();
        User user = usersService.selectOne(new EntityWrapper<User>().eq("username", userName));
        if (ObjectUtils.isEmpty(user)) {
            log.error("查询用户失败！调用{}的{}方法出错 ", "AppServiceImpl", "getAppListByUser()");
            throw new BusinessException(ResCodeEnum.USER_NOT_EXIST, ExceptionMessageEnum.USER_NO_SUCH_VALUE);
        }
        // 根据用户id和企业id查询用户企业信息
        int userId = user.getId();
        EnterprisesUsers relation = enterprisesUsersService.selectOne(new EntityWrapper<EnterprisesUsers>()
                .eq("user_id", userId)
                .eq("enterprise_id", orgId));
        List<AppDTO> appDTOList = new ArrayList<>();
        if (!ObjectUtils.isEmpty(relation) && StringUtils.hasLength(relation.getAppIds())) {
            String[] appIds = StringUtils.split(relation.getAppIds(), ",");
            if (!ObjectUtils.isEmpty(appIds)) {
                for (String appId : appIds) {
                    int aId = Integer.parseInt(appId);
                    App app = selectById(aId);
                    AppDTO appDTO = BeanUtils.copyProperties(app, AppDTO.class);
                    appDTO.setOrgId(orgId);
                    // 根据企业id和appID查询企业应用信息
                    EnterprisesApps enterprisesApps = enterprisesAppsService.selectOne(new EntityWrapper<EnterprisesApps>()
                            .eq("org_id", orgId)
                            .eq("app_id", aId));
                    if (!ObjectUtils.isEmpty(enterprisesApps) && !ObjectUtils.isEmpty(enterprisesApps.getUrl())) {
                        appDTO.setUrl(enterprisesApps.getUrl());
                    }
                    appDTO.setStatus(1);
                    appDTOList.add(appDTO);
                }
            }
        }
        return appDTOList;
    }

    /**
     * 用户开通应用
     *
     * @param userInfo
     * @param userIds
     * @param appIds
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult addAppToUser(AuthPlatformUserInfo userInfo, int[] userIds, int[] appIds) {

        if (ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("获取企业id失败");
            return ResponseResult.buildResponseResult(ResCodeEnum.NOT_FOUND_COMPANY_ID, ExceptionMessageEnum.ENTERPRISE_ID_NOT_EXIST);
        }
        // 根据企业id和用户id查询企业用户信息
        int orgId = userInfo.getOrgId();
        for (int userId : userIds) {
            EnterprisesUsers relation = enterprisesUsersService.selectOne(new EntityWrapper<EnterprisesUsers>()
                    .eq("user_id", userId)
                    .eq("enterprise_id", orgId));
            if (!ObjectUtils.isEmpty(relation)) {
                String ids = relation.getAppIds() == null ? "" : relation.getAppIds();
                List<String> idList = new ArrayList<>();
                if (!StringUtils.isEmpty(ids)) {
                    idList = Arrays.asList(ids.split(","));
                }
                List<String> newIdList = new ArrayList<>();
                for (int appId : appIds) {
                    // 根据appIds查询应用信息
                    if (!ObjectUtils.isEmpty(this.selectById(appId))) {
                        newIdList.add(String.valueOf(appId));
                    } else {
                        log.error("请求参数appIds异常,无此应用！调用{}的{}方法出错 ", "AppServiceImpl", "addAppToUser()");
                        throw new BusinessException(ResCodeEnum.NO_DATA, ExceptionMessageEnum.APP_ID_PARAMS_NOT_FOUND);
                    }
                }
                Set<String> idSet = new HashSet<>(idList);
                idSet.addAll(newIdList);
                idSet.removeAll(Arrays.asList("", null));
                String idStr = String.join(",", idSet);
                // 添加用户应用信息
                synchronized (this) {
                    if (!enterprisesUsersService.update(new EnterprisesUsers().setAppIds(idStr),
                            new EntityWrapper<EnterprisesUsers>()
                                    .eq("user_id", userId)
                                    .eq("enterprise_id", orgId))) {
                        log.error("修改用户企业app_ids失败！调用{}的{}方法出错 ", "AppServiceImpl", "addAppToUser()");
                        throw new BusinessException(ResCodeEnum.NO_DATA, ExceptionMessageEnum.UPDATE_APP_ID_FAILED);
                    }
                }
            } else {
                log.error("无法查询到企业用户信息！调用{}的{}方法出错 ", "AppServiceImpl", "addAppToUser()");
                throw new BusinessException(ResCodeEnum.GET_ENTERPRISE_USER_RELATION_FAILED, ExceptionMessageEnum.USER_ID_PARAMS_NOT_FOUND);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS.code, "应用开通成功");
    }

    /**
     * 移除用户应用
     *
     * @param userInfo
     * @param userIds
     * @param appIds
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult removeAppFromUser(AuthPlatformUserInfo userInfo, int[] userIds, int[] appIds) {

        if (ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("获取企业id失败");
            throw new BusinessException(ResCodeEnum.NOT_FOUND_COMPANY_ID, ExceptionMessageEnum.ENTERPRISE_ID_NOT_EXIST);
        }
        // 根据企业id和用户id查询企业用户信息
        int orgId = userInfo.getOrgId();
        for (int userId : userIds) {
            EnterprisesUsers relation = enterprisesUsersService.selectOne(new EntityWrapper<EnterprisesUsers>()
                    .eq("user_id", userId)
                    .eq("enterprise_id", orgId));
            if (!ObjectUtils.isEmpty(relation) && !ObjectUtils.isEmpty(relation.getAppIds())) {
                StringBuffer sb = new StringBuffer(relation.getAppIds());
                for (int appId : appIds) {
                    // 根据请求appIds查询应用信息
                    if (ObjectUtils.isEmpty(this.selectById(appId))) {
                        log.error("参数异常,无此应用,调用{}的{}方法出错", "AppServiceImpl", "removeAppFromUser");
                        throw new BusinessException(ResCodeEnum.NO_DATA, ExceptionMessageEnum.APP_ID_PARAMS_NOT_FOUND);
                    }
                    if (Arrays.asList(sb.toString().split(",")).contains(String.valueOf(appId))) {
                        if (sb.length() == 1) {
                            // 如果只开通了即将删除的一个应用，sb应该类似于：sb="1"
                            sb.deleteCharAt(0);
                        } else {
                            // 如果开通了很多应用，获取即将删除应用在sb中下标，如果下标为0，则删除id和后面的逗号
                            int index = sb.indexOf(String.valueOf(appId));
                            if (index == 0) {
                                sb.delete(0, 2);
                            } else {
                                sb.delete(index - 1, index + 1);
                            }
                        }
                    } else {
                        log.error("参数异常,该用户无此应用,调用{}的{}方法出错", "AppServiceImpl", "removeAppFromUser");
                        throw new BusinessException(ResCodeEnum.NO_DATA, ExceptionMessageEnum.APP_ID_PARAMS_NOT_FOUND);

                    }
                }
                // 移除用户应用信息
                synchronized (this) {
                    if (enterprisesUsersMapper.update(new EnterprisesUsers().setAppIds(sb.toString()),
                            new EntityWrapper<EnterprisesUsers>()
                                    .eq("user_id", userId)
                                    .eq("enterprise_id", orgId)) != 1) {
                        log.error("修改用户企业app_ids失败！调用{}的{}方法出错 ", "AppServiceImpl", "removeAppFromUser()");
                        throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.UPDATE_APP_ID_FAILED);
                    }
                }

            } else {
                log.error("无法查询到企业用户信息！调用{}的{}方法出错 ", "AppServiceImpl", "removeAppFromUser()");
                throw new BusinessException(ResCodeEnum.UPDATE_FAIL, ExceptionMessageEnum.USER_ID_PARAMS_NOT_FOUND);
            }
        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS.code,"关闭应用成功");

    }

    /**
     * 查询app产品列表
     * @return
     */
    @Override
    public List<AppDTO> getAllAppsList() {
        List<App> appList = this.selectList(new EntityWrapper<App>()
                .eq("status", EnumCommon.IsActive.is_active.getKey()));
        List<AppDTO> appDTOList = new ArrayList<>();
        if (!CollectionUtils.isEmpty(appList)) {
            for (App app : appList) {
                AppDTO appDTO = new AppDTO();
                appDTO.setId(app.getId());
                appDTO.setName(app.getName());
                appDTOList.add(appDTO);
            }
        }
        return appDTOList;
    }
}

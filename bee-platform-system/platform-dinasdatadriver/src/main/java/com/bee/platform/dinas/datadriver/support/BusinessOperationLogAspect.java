 package com.bee.platform.dinas.datadriver.support;

 import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.ReflectionUtils.FieldCallback;
import org.springframework.util.ReflectionUtils.FieldFilter;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.dinas.datadriver.entity.DinasOperationLog;
import com.bee.platform.dinas.datadriver.service.DinasOperationLogService;
import com.bee.platform.dinas.datadriver.support.annotation.BusinessId;
import com.bee.platform.dinas.datadriver.support.annotation.Log;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Aspect
@Component
 public class BusinessOperationLogAspect {

    @Autowired
    DinasOperationLogService logService;

    @Autowired
    UserInfoUtils userInfoUtils;


    @AfterReturning(pointcut = "execution(public * com.bee.platform.dinas.datadriver.controller..*.*(..)) "
        + "&& @annotation(com.bee.platform.dinas.datadriver.support.annotation.Log)",
        returning = "ret")
    public void log(JoinPoint jp, Object ret) {
        MethodSignature ms = (MethodSignature)jp.getSignature();
        Method method = ms.getMethod();

        // 非正常流程返回,不记录日志
        if (ResponseResult.class.isAssignableFrom(ret.getClass())) {
            ResponseResult rr = (ResponseResult)ret;
            if (!ResCodeEnum.SUCCESS.getCode().equals(rr.getCode())) {
                return;
            }
        }

        Log logAnnotation = method.getAnnotation(Log.class);
        OperateType opType = logAnnotation.operateType();

        Object businessId = getBusinessId(jp, ret, method);
        if (ObjectUtils.isEmpty(businessId)) {
            log.error("未获取到businessId,无法正常记录日志");
            throw new IllegalStateException("未获取到businessId,无法正常记录日志");
        }
        AuthPlatformUserInfo user = getUser();
        if (user == null) {
            log.error("记录操作日志出错,获取用户信息失败");
        } else {
            try {
                if (logAnnotation.isBatch()) {
                    if (List.class.isAssignableFrom(businessId.getClass())) {
                        List<DinasOperationLog> logList = ((List<Object>)businessId).stream().map(s -> newLog(logAnnotation, opType, (Integer)s, user)).collect(Collectors.toList());
                        Optional.ofNullable(logList).filter(o -> !CollectionUtils.isEmpty(o)).map(o -> logService.insertBatch(o));
                    } else {
                        log.error("批量记录操作日志出错,businessId 类型不为List");
                    }
                } else {
                    newLog(logAnnotation, opType, (Integer)businessId, user).insert();
                }
            } catch (Exception e) {
                log.error("记录操作日志出错", e);
            }
        }

    }

    /**
     * 解析方法参数,获取businessId
     * @param jp
     * @param ret
     * @param method
     * @return
     */
    private Object getBusinessId(JoinPoint jp, Object ret, Method method) {
        Object businessId = getBusinessIdFromReturnVal(ret);

        // 返回值中未获取到businessId,则从方法参数中获取
        if (businessId == null) {
            businessId = getBusinessIdFromArgs(jp, method);
        }
        return businessId;
    }

    /**
     * 解析返回值,获取businessId
     * @param ret
     * @return
     */
    private Object getBusinessIdFromReturnVal(Object ret) {
        Object businessId = null;
        // 解析返回值, 获取businessId
        Object dest = null;
        if (ret.getClass().equals(ResponseResult.class)) {
            dest = ((ResponseResult<?>) ret).getObject();
        } else {
            dest = ret;
        }

        // 数据类型为Integer或List,则视为businessId
        if (Objects.nonNull(dest)) {
            if (Integer.class.isAssignableFrom(dest.getClass())
                    || List.class.isAssignableFrom(dest.getClass())) {
                businessId = dest;
            } 
            // 否则遍历对象属性获取
            else {
                businessId = queryBusinessIdOfObj(dest);
            }
        }
        return businessId;
    }

    /**
     * 构建log对象
     * @param logAnnotation
     * @param opType
     * @param businessId
     * @param user
     * @return
     */
    private DinasOperationLog newLog(Log logAnnotation, OperateType opType, Integer businessId, AuthPlatformUserInfo user) {
        DinasOperationLog opLog = new DinasOperationLog();
        String msg = logAnnotation.msg();
        opLog.setOperateTime(new Date());
        opLog.setOperateMsg(StringUtils.isNotBlank(msg) ? msg : opType.getMsg());
        opLog.setBusinessId(businessId);
        opLog.setBusinessType(logAnnotation.businessType().getCode());
        opLog.setOperator(user.getId());
        opLog.setCompanyId(user.getOrgId());
        opLog.setOperatorName(user.getName());
        return opLog;
    }

    /**
     * 获取当前用户
     * @return
     */
    private AuthPlatformUserInfo getUser() {
        RequestAttributes currentRequestAttributes = RequestContextHolder.currentRequestAttributes();
        HttpServletRequest request = ((ServletRequestAttributes)currentRequestAttributes).getRequest();
        try {

            AuthPlatformUserInfo user = userInfoUtils.getSimpleUserInfo(request);
            return user;
        } catch (Exception e) {
            log.error("获取用户信息失败", e);
        }
        return null;
    }

    /**
     * 获取业务id
     * @param jp
     * @param method
     * @return
     */
    private Object getBusinessIdFromArgs(JoinPoint jp, Method method) {
        Object[] args = jp.getArgs();
        Parameter[] params = method.getParameters();
        Object businessId = null;
        for (int i = 0; i < args.length; i++) {
            if (params[i].isAnnotationPresent(BusinessId.class)) {
                if (Integer.class.isAssignableFrom(args[i].getClass())
                    || List.class.isAssignableFrom(args[i].getClass())) {
                    businessId = args[i] == null ? null : args[i];
                } else {
                    Object businessObj = args[i];
                    businessId = queryBusinessIdOfObj(businessObj);
                }
                break;
            }
        }
        return businessId;
    }

    /**
     * 获取对象内第一个标记为@BusinessId注解的属性
     * @param businessObj
     * @return
     */
    private Object queryBusinessIdOfObj(Object businessObj) {
        final Object[] ids = new Object[1];
        ReflectionUtils.doWithFields(businessObj.getClass(), new FieldCallback() {
            @Override
            public void doWith(Field field) throws IllegalArgumentException, IllegalAccessException {
                field.setAccessible(true);
                ids[0] = field.get(businessObj) == null ? null : field.get(businessObj);
            }
        }, new FieldFilter() {
            @Override
            public boolean matches(Field field) {
                return field.isAnnotationPresent(BusinessId.class);
            }
        });
        return ids[0];
    }
}
